open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; set_state : 'a [@bits 1]
    ; start_round : 'a [@bits 1]
    ; round_input : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { finished : 'a [@bits 1]
    ; round_output : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module States = struct
  type t =
    | Waiting
    | Update_state
    | Starting_block
    | Processing_block
    | Xor
  [@@deriving sexp_of, compare, enumerate]
end

let create scope ({ clock; clear; set_state; start_round; round_input } : Signal.t I.t) =
  let open Always in
  let open Variable in
  let r_sync = Reg_spec.create ~clock ~clear () in
  let finished = wire ~default:gnd in
  let current_state = reg ~enable:vdd ~width:512 r_sync in
  let input = reg ~enable:vdd ~width:512 r_sync in
  let output = reg ~enable:vdd ~width:512 r_sync in
  let state_counter =
    Util.select_byte_range current_state.value ~from:(4 * 12) ~to_:(4 * 13)
  in
  let block_start = wire ~default:gnd in
  let { Chacha20_pipelined_block.O.round_output = block_output
      ; finished = block_finished
      }
    =
    Chacha20_pipelined_block.hierarchical
      ~instance:(Scope.name scope "pipelined_block")
      scope
      { Chacha20_pipelined_block.I.round_input = current_state.value
      ; clock
      ; clear
      ; start = block_start.value
      }
  in
  let state = Always.State_machine.create (module States) ~enable:vdd r_sync in
  compile
    [ state.switch
        [ ( Waiting
          , [ finished <--. 1
            ; when_
                (start_round ==:. 0 &: set_state ==:. 1)
                [ current_state <-- round_input ]
            ; when_
                (start_round ==:. 1)
                [ input <-- round_input; state.set_next Update_state ]
            ] )
        ; ( Update_state
          , [ current_state
              <-- Util.replace_byte_range
                    ~from:(4 * 12)
                    ~to_:(4 * 13)
                    ~with_:(state_counter +:. 1)
                    current_state.value
            ; state.set_next Starting_block
            ] )
        ; Starting_block, [ block_start <--. 1; state.set_next Processing_block ]
        ; Processing_block, [ when_ (block_finished ==:. 1) [ state.set_next Xor ] ]
        ; Xor, [ output <-- input.value ^: block_output; state.set_next Waiting ]
        ]
    ];
  { O.finished = finished.value; round_output = output.value }
;;

let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"chacha20_serial_encoder" ~instance create input
;;

let%test_module "IETF sunblock test" =
  (* Described at https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.2 *)
  (module struct
    let process_whole_block ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
      printf "Start of block: \n";
      Util.bytestring_of_bits !(inputs.round_input) |> Util.hexdump;
      inputs.start_round := Bits.vdd;
      Cyclesim.cycle sim;
      inputs.start_round := Bits.gnd;
      let cycles = ref 1 in
      while Bits.to_int !(outputs.finished) = 0 do
        Cyclesim.cycle sim;
        cycles := !cycles + 1
      done;
      printf "Output (Real Output): \n";
      Util.bytestring_of_bits !(outputs.round_output) |> Util.hexdump;
      printf "Cycles: %i\n" !cycles
    ;;

    let example_text_bits =
      "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for \
       the future, sunscreen would be it."
      |> String.to_list
      |> List.map ~f:Bits.of_char
      |> Bits.concat_lsb
    ;;

    let%expect_test "encrypt and decrypt a two block passage about suncream" =
      let module Simulator = Cyclesim.With_interface (I) (O) in
      let sim = Simulator.create (create (Scope.create ~flatten_design:true ())) in
      let inputs : _ I.t = Cyclesim.inputs sim in
      let outputs : _ O.t = Cyclesim.outputs sim in
      (* Set the initial state for encryption *)
      inputs.set_state := Bits.of_int ~width:1 1;
      inputs.round_input
      := Util.ietf_example_initial_state ~nonce:Util.sunscreen_nonce ~counter:0;
      Cyclesim.cycle sim;
      inputs.set_state := Bits.of_int ~width:1 0;
      printf "Processing blocks\n";
      inputs.round_input := Bits.select example_text_bits 511 0;
      process_whole_block ~sim ~inputs ~outputs;
      let first_block_ciphertext = Bits.to_string !(outputs.round_output) in
      [%expect
        {|
        Processing blocks
        Start of block:
        001: 4c 61 64 69 65 73 20 61 6e 64 20 47 65 6e 74 6c | Ladies and Gentl
        002: 65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73 | emen of the clas
        003: 73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63 | s of '99: If I c
        004: 6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f | ould offer you o
        005:                                                 |
        Output (Real Output):
        001: 6e 2e 35 9a 25 68 f9 80 41 ba 07 28 dd 0d 69 81 | n.5.%h..A..(.\ri.
        002: e9 7e 7a ec 1d 43 60 c2 0a 27 af cc fd 9f ae 0b | .~z..C`.\n'......
        003: f9 1b 65 c5 52 47 33 ab 8f 59 3d ab cd 62 b3 57 | ..e.RG3..Y=..b.W
        004: 16 39 d6 24 e6 51 52 ab 8f 53 0c 35 9f 08 61 d8 | .9.$.QR..S.5..a.
        005:                                                 |
        Cycles: 16 |}];
      inputs.round_input
      := Bits.concat_lsb
           [ Bits.select example_text_bits 911 512; Bits.of_int ~width:(1024 - 912) 0 ];
      process_whole_block ~sim ~inputs ~outputs;
      let second_block_ciphertext = Bits.to_string !(outputs.round_output) in
      [%expect
        {|
        Start of block:
        001: 6e 6c 79 20 6f 6e 65 20 74 69 70 20 66 6f 72 20 | nly one tip for
        002: 74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73 | the future, suns
        003: 63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69 | creen would be i
        004: 74 2e 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | t...............
        005:                                                 |
        Output (Real Output):
        001: 07 ca 0d bf 50 0d 6a 61 56 a3 8e 08 8a 22 b6 5e | ..\r.P\rjaV....".^
        002: 52 bc 51 4d 16 cc f8 06 81 8c e9 1a b7 79 37 36 | R.QM.........y76
        003: 5a f9 0b bf 74 a3 5b e6 b4 0b 8e ed f2 78 5e 42 | Z...t.[......x^B
        004: 87 4d 74 03 73 20 1a a1 88 fb bc e8 39 91 c4 ed | .Mt.s ......9...
        005:                                                 |
        Cycles: 16 |}];
      (* Resetting the state for decryption *)
      inputs.set_state := Bits.of_int ~width:1 1;
      inputs.round_input
      := Util.ietf_example_initial_state ~nonce:Util.sunscreen_nonce ~counter:0;
      Cyclesim.cycle sim;
      printf "Reset state for decryption\n";
      (* Decrypt first block *)
      inputs.set_state := Bits.of_int ~width:1 0;
      inputs.round_input := Bits.of_string first_block_ciphertext;
      process_whole_block ~sim ~inputs ~outputs;
      [%expect
        {|
        Reset state for decryption
        Start of block:
        001: 6e 2e 35 9a 25 68 f9 80 41 ba 07 28 dd 0d 69 81 | n.5.%h..A..(.\ri.
        002: e9 7e 7a ec 1d 43 60 c2 0a 27 af cc fd 9f ae 0b | .~z..C`.\n'......
        003: f9 1b 65 c5 52 47 33 ab 8f 59 3d ab cd 62 b3 57 | ..e.RG3..Y=..b.W
        004: 16 39 d6 24 e6 51 52 ab 8f 53 0c 35 9f 08 61 d8 | .9.$.QR..S.5..a.
        005:                                                 |
        Output (Real Output):
        001: 4c 61 64 69 65 73 20 61 6e 64 20 47 65 6e 74 6c | Ladies and Gentl
        002: 65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73 | emen of the clas
        003: 73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63 | s of '99: If I c
        004: 6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f | ould offer you o
        005:                                                 |
        Cycles: 16 |}];
      (* Decrypt second block *)
      inputs.set_state := Bits.of_int ~width:1 0;
      inputs.round_input := Bits.of_string second_block_ciphertext;
      process_whole_block ~sim ~inputs ~outputs;
      [%expect
        {|
        Start of block:
        001: 07 ca 0d bf 50 0d 6a 61 56 a3 8e 08 8a 22 b6 5e | ..\r.P\rjaV....".^
        002: 52 bc 51 4d 16 cc f8 06 81 8c e9 1a b7 79 37 36 | R.QM.........y76
        003: 5a f9 0b bf 74 a3 5b e6 b4 0b 8e ed f2 78 5e 42 | Z...t.[......x^B
        004: 87 4d 74 03 73 20 1a a1 88 fb bc e8 39 91 c4 ed | .Mt.s ......9...
        005:                                                 |
        Output (Real Output):
        001: 6e 6c 79 20 6f 6e 65 20 74 69 70 20 66 6f 72 20 | nly one tip for
        002: 74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73 | the future, suns
        003: 63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69 | creen would be i
        004: 74 2e 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | t...............
        005:                                                 |
        Cycles: 16 |}]
    ;;
  end)
;;
