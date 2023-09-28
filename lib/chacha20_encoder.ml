open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; set_state : 'a [@bits 1]
    ; input : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create ({ clock; clear; set_state; input } : Signal.t I.t) =
  let open Always in
  let open Variable in
  let r_sync = Reg_spec.create ~clock ~clear () in
  let current_state = reg ~enable:vdd ~width:512 r_sync in
  let state_counter =
    Util.select_byte_range current_state.value ~from:(4 * 12) ~to_:(4 * 13)
  in
  let block_output =
    Chacha20_block.create { Chacha20_block.I.input = current_state.value }
  in
  let encode_logic =
    [ current_state
      <-- Util.replace_byte_range
            ~from:(4 * 12)
            ~to_:(4 * 13)
            ~with_:(state_counter +:. 1)
            current_state.value
    ]
  in
  let body = [ if_ (set_state ==:. 1) [ current_state <-- input ] encode_logic ] in
  compile body;
  { O.output = input ^: block_output.output }
;;

let%test_module "Functional test" =
  (module struct
    let cycle_and_print ~sim ~(outputs : _ O.t) =
      printf "Start of cycle\n";
      Cyclesim.cycle sim;
      printf "Output (Real Output): \n";
      Util.print_state !(outputs.output)
    ;;

    let%expect_test "fixed test input" =
      let module Simulator = Cyclesim.With_interface (I) (O) in
      let sim = Simulator.create create in
      let inputs : _ I.t = Cyclesim.inputs sim in
      let outputs : _ O.t = Cyclesim.outputs sim in
      let input_state =
        Util.ietf_example_initial_state ~nonce:Util.sunscreen_nonce ~counter:1
      in
      printf "Setting the initial state\n";
      inputs.set_state := Bits.of_int ~width:1 1;
      inputs.input := input_state;
      cycle_and_print ~sim ~outputs;
      [%expect
        {|
        Setting the initial state
        Start of cycle
        Output (Real Output):
         00: 92213747 01: d2f97f2e 02: 1645f31d 03: 863d06cc
         04: 811d128c 05: e5002939 06: e7c04676 07: 77c1fe92
         08: b0182a9a 09: 851c7566 10: d66e60ad 11: 2b8d36f1
         12: 40ba4c78 13: cd343ec6 14: 062c21ea 15: b7417df0 |}];
      printf "Doing a single encode with the state we just set\n";
      inputs.set_state := Bits.of_int ~width:1 0;
      cycle_and_print ~sim ~outputs;
      [%expect
        {|
        Doing a single encode with the state we just set
        Start of cycle
        Output (Real Output):
         00: fe04de0c 01: 722f0751 02: 519ce710 03: 15e42898
         04: 6e36d526 05: 748abc74 06: 31cfe0fb 07: 4a5701c8
         08: c97c9a29 09: 9e3a960e 10: d6f07ed9 11: 3460008c
         12: 037463f2 13: a11a2073 14: a2bcfb88 15: edc49139 |}];
      printf
        "Processing the same ciphertext again and observing that we get entirely \
         different output because it is XOR'd with a new state\n";
      cycle_and_print ~sim ~outputs;
      [%expect
        {|
        Processing the same ciphertext again and observing that we get entirely different output because it is XOR'd with a new state
        Start of cycle
        Output (Real Output):
         00: 2d7695ad 01: 73bdf700 02: 988c5803 03: 8ed33533
         04: 4589afa9 05: 4dab48c5 06: a4c0219e 07: df2f470a
         08: be41631a 09: a798daec 10: 73247580 11: 7ac0db6e
         12: cb76804c 13: 179ca19a 14: f6863d41 15: 136bd50e |}]
    ;;
  end)
;;

let%test_module "IETF sunblock test" =
  (* Described at https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.2 *)
  (module struct
    let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
      printf "Start of cycle\n";
      Util.bytestring_of_bits !(inputs.input) |> Util.hexdump;
      Cyclesim.cycle sim;
      printf "Output (Real Output): \n";
      Util.bytestring_of_bits !(outputs.output) |> Util.hexdump
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
      let sim = Simulator.create create in
      let inputs : _ I.t = Cyclesim.inputs sim in
      let outputs : _ O.t = Cyclesim.outputs sim in
      (* Set the initial state for encryption *)
      inputs.set_state := Bits.of_int ~width:1 1;
      inputs.input
      := Util.ietf_example_initial_state ~nonce:Util.sunscreen_nonce ~counter:0;
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
        Start of cycle
        001: 65 78 70 61 6e 64 20 33 32 2d 62 79 74 65 20 6b | expand 32-byte k
        002: 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f | ..........\n..\r..
        003: 10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f | ................
        004: 00 00 00 00 00 00 00 00 00 00 00 4a 00 00 00 00 | ...........J....
        005:                                                 |
        Output (Real Output):
        001: ca 7d 6e 21 d5 c4 15 7a b3 1f f8 f9 1e 71 2e c4 | .}n!...z.....q..
        002: d2 59 a0 29 69 ce 4d be fe 5f 96 b8 e3 ef d0 a0 | .Y.)i.M.._......
        003: 93 6a ca 6f b6 1e 4d b6 38 98 b9 1d b3 13 ad 43 | .j.o..M.8......C
        004: 41 a2 39 d2 0d fc 74 c8 17 71 56 47 9c 9c 1e 4b | A.9.\r.t..qVG...K
        005:                                                 | |}];
      inputs.set_state := Bits.of_int ~width:1 0;
      inputs.input := Bits.select example_text_bits 511 0;
      cycle_and_print ~sim ~inputs ~outputs;
      let first_block_ciphertext = Bits.to_string !(outputs.output) in
      [%expect
        {|
        Start of cycle
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
        005:                                                 | |}];
      inputs.input
      := Bits.concat_lsb
           [ Bits.select example_text_bits 911 512; Bits.of_int ~width:(1024 - 912) 0 ];
      cycle_and_print ~sim ~inputs ~outputs;
      let second_block_ciphertext = Bits.to_string !(outputs.output) in
      [%expect
        {|
        Start of cycle
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
        005:                                                 | |}];
      (* Resetting the state for decryption *)
      inputs.set_state := Bits.of_int ~width:1 1;
      inputs.input
      := Util.ietf_example_initial_state ~nonce:Util.sunscreen_nonce ~counter:0;
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
        Start of cycle
        001: 65 78 70 61 6e 64 20 33 32 2d 62 79 74 65 20 6b | expand 32-byte k
        002: 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f | ..........\n..\r..
        003: 10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f | ................
        004: 00 00 00 00 00 00 00 00 00 00 00 4a 00 00 00 00 | ...........J....
        005:                                                 |
        Output (Real Output):
        001: ca 7d 6e 21 d5 c4 15 7a b3 1f f8 f9 1e 71 2e c4 | .}n!...z.....q..
        002: d2 59 a0 29 69 ce 4d be fe 5f 96 b8 e3 ef d0 a0 | .Y.)i.M.._......
        003: 93 6a ca 6f b6 1e 4d b6 38 98 b9 1d b3 13 ad 43 | .j.o..M.8......C
        004: 41 a2 39 d2 0d fc 74 c8 17 71 56 47 9c 9c 1e 4b | A.9.\r.t..qVG...K
        005:                                                 | |}];
      (* Decrypt first block *)
      inputs.set_state := Bits.of_int ~width:1 0;
      inputs.input := Bits.of_string first_block_ciphertext;
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
        Start of cycle
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
        005:                                                 | |}];
      (* Decrypt second block *)
      inputs.set_state := Bits.of_int ~width:1 0;
      inputs.input := Bits.of_string second_block_ciphertext;
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
        Start of cycle
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
        005:                                                 | |}]
    ;;
  end)
;;
