open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; set_state : 'a [@bits 1]
    ; input_state : 'a [@bits 512]
    ; encode : 'a [@bits 1]
    ; encode_data : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { input_state_for_debugging : 'a [@bits 512]
    ; output_state : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

let replace ~hi ~lo ~with_ signal =
  let before = select signal (lo - 1) 0 in
  let after = select signal (Signal.width signal - 1) (hi + 1) in
  concat_lsb [ before; with_; after ]
;;

let create ({ clock; clear; set_state; input_state; encode; encode_data } : Signal.t I.t) =
  let open Always in
  let open Variable in
  let r_sync = Reg_spec.create ~clock ~clear () in
  let current_state = reg ~enable:vdd ~width:512 r_sync in
  let output_state = wire ~default:(Signal.of_int ~width:512 0) in
  let ctr_lo_bit = 32 * 12 in
  let ctr_hi_bit = (32 * 13) - 1 in
  let state_counter = select current_state.value ctr_hi_bit ctr_lo_bit in
  Core.print_s [%message (Signal.width state_counter : int)];
  let block_output =
    Chacha20_block.create { Chacha20_block.I.input_state = current_state.value }
  in
  let body =
    [ if_
        (set_state ==:. 1)
        [ current_state <-- input_state ]
        [ if_
            (encode ==:. 1)
            [ output_state <-- encode_data ^: block_output.output_state
            ; current_state
              <-- replace
                    ~hi:ctr_hi_bit
                    ~lo:ctr_lo_bit
                    ~with_:(state_counter +:. 1)
                    current_state.value
            ]
            []
        ]
    ]
  in
  compile body;
  { O.output_state = output_state.value; input_state_for_debugging = current_state.value }
;;

let%test_module "Basic tests" =
  (module struct
    let print_state bits =
      Sequence.range 0 16
      |> Sequence.iter ~f:(fun word ->
           let word_bits = Bits.select bits ((word * 32) + 31) (word * 32) in
           printf " %i: %x" word (Bits.to_int word_bits);
           if (word + 1) % 4 = 0 then printf "\n";
           ())
    ;;

    let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
      printf "Start of cycle\n";
      printf "Input: \n";
      print_state !(inputs.input_state);
      Cyclesim.cycle sim;
      printf "Output (Real Output): \n";
      print_state !(outputs.output_state);
      printf "Output (Input State For Debugging): \n";
      print_state !(outputs.input_state_for_debugging)
    ;;

    let%expect_test "fixed test input" =
      let module Simulator = Cyclesim.With_interface (I) (O) in
      let sim = Simulator.create create in
      let inputs : _ I.t = Cyclesim.inputs sim in
      let outputs : _ O.t = Cyclesim.outputs sim in
      let input_state =
        let example_key =
          let w1 = [ 0x0; 0x01; 0x02; 0x03 ] in
          let w2 = [ 0x04; 0x05; 0x06; 0x07 ] in
          let w3 = [ 0x08; 0x09; 0x0a; 0x0b ] in
          let w4 = [ 0x0c; 0x0d; 0x0e; 0x0f ] in
          let w5 = [ 0x10; 0x11; 0x12; 0x13 ] in
          let w6 = [ 0x14; 0x15; 0x16; 0x17 ] in
          let w7 = [ 0x18; 0x19; 0x1a; 0x1b ] in
          let w8 = [ 0x1c; 0x1d; 0x1e; 0x1f ] in
          w1 @ w2 @ w3 @ w4 @ w5 @ w6 @ w7 @ w8 |> List.map ~f:Char.of_int_exn
        in
        let example_nonce =
          [ 00; 0x00; 0x00; 0x09; 0x00; 0x00; 0x00; 0x4a; 0x00; 0x00; 0x00; 0x00 ]
          |> List.map ~f:Char.of_int_exn
        in
        Util.create_state ~key:example_key ~nonce:example_nonce
        |> List.map ~f:Bits.of_char
        |> Bits.concat_lsb
      in
      printf "Setting the initial state\n";
      inputs.set_state := Bits.of_int ~width:1 1;
      inputs.encode := Bits.of_int ~width:1 0;
      inputs.input_state := input_state;
      cycle_and_print ~sim ~inputs ~outputs;
      printf "Doing a single encode with the state we just set\n";
      inputs.set_state := Bits.of_int ~width:1 0;
      inputs.encode := Bits.of_int ~width:1 1;
      (* 1 2 3 4 5 ... as each word in the ciphertext. *)
      inputs.encode_data
        := List.init ~f:(fun i -> Char.of_int_exn i |> Bits.of_char) 64 |> Bits.concat_lsb;
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
        ("Signal.width state_counter" 32)
        Setting the initial state
        Start of cycle
        Input:
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 1 13: 9000000 14: 4a000000 15: 0
        Output (Real Output):
         0: 0 1: 0 2: 0 3: 0
         4: 0 5: 0 6: 0 7: 0
         8: 0 9: 0 10: 0 11: 0
         12: 0 13: 0 14: 0 15: 0
        Output (Input State For Debugging):
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 1 13: 9000000 14: 4a000000 15: 0
        Doing a single encode with the state we just set
        Start of cycle
        Input:
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 1 13: 9000000 14: 4a000000 15: 0
        Output (Real Output):
         0: 7481890a 1: 49b9d23d 2: bba6c5f0 3: d9b726e6
         4: 87d1478d 5: ea0b20be 6: 845fa6bd 7: f7813316
         8: b1da00c7 9: a1e2dc71 10: b74d0897 11: b3611044
         12: 14c8c36c 13: 371060b2 14: cf03f63 15: 491bb70
        Output (Input State For Debugging):
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 2 13: 9000000 14: 4a000000 15: 0 |}]
    ;;
  end)
;;
