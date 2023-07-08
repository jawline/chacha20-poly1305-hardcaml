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
        Util.ietf_example_initial_state
      in
      printf "Setting the initial state\n";
      inputs.set_state := Bits.of_int ~width:1 1;
      inputs.encode := Bits.of_int ~width:1 0;
      inputs.input_state := input_state;
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
         12: 1 13: 9000000 14: 4a000000 15: 0 |}];
      printf "Doing a single encode with the state we just set\n";
      inputs.set_state := Bits.of_int ~width:1 0;
      inputs.encode := Bits.of_int ~width:1 1;
      (* 1 2 3 4 5 ... as each word in the ciphertext. *)
      inputs.encode_data
        := List.init ~f:(fun i -> Char.of_int_exn i |> Bits.of_char) 64 |> Bits.concat_lsb;
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
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
         12: 2 13: 9000000 14: 4a000000 15: 0 |}];
      printf
        "Processing the same ciphertext again and observing that we get entirely \
         different output because it is XOR'd with a new state\n";
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
        Processing the same ciphertext again and observing that we get entirely different output because it is XOR'd with a new state
        Start of cycle
        Input:
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 1 13: 9000000 14: 4a000000 15: 0
        Output (Real Output):
         0: c8bfbedc 1: 8163bb87 2: 5c8dc26 3: 2b4d57a2
         4: c9807b0d 5: 28cdf79 6: 8c48fb73 7: 73901e9
         8: b03ca7aa 9: 35cd1fe8 10: a0735fb2 11: 6a09e080
         12: 28a6f70a 13: 66287b7f 14: d2c4906b 15: 319e2661
        Output (Input State For Debugging):
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 3 13: 9000000 14: 4a000000 15: 0 |}]
    ;;
  end)
;;

let%test_module "IETF Test" =
  (* Described at https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.2 *)
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
      Util.bytestring_of_bits !(outputs.output_state) |> Util.hexdump;
      printf "Output (Input State For Debugging): \n";
      print_state !(outputs.input_state_for_debugging)
    ;;

    let%expect_test "fixed test input" =
      let module Simulator = Cyclesim.With_interface (I) (O) in
      let sim = Simulator.create create in
      let inputs : _ I.t = Cyclesim.inputs sim in
      let outputs : _ O.t = Cyclesim.outputs sim in
            printf "Setting the initial state\n";
      inputs.set_state := Bits.of_int ~width:1 1;
      inputs.encode := Bits.of_int ~width:1 0;
      inputs.input_state := Util.ietf_example_initial_state;
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
        001: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
        002: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
        003: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
        004: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
        005:                                                 |
        Output (Input State For Debugging):
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 1 13: 9000000 14: 4a000000 15: 0 |}];
      printf "Doing a single encode with the state we just set\n";
      inputs.set_state := Bits.of_int ~width:1 0;
      inputs.encode := Bits.of_int ~width:1 1;
      (* 1 2 3 4 5 ... as each word in the ciphertext. *)
      inputs.encode_data
        := List.init ~f:(fun i -> Char.of_int_exn i |> Bits.of_char) 64 |> Bits.concat_lsb;
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
        Doing a single encode with the state we just set
        Start of cycle
        Input:
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 1 13: 9000000 14: 4a000000 15: 0
        Output (Real Output):
        001: 0a 89 81 74 3d d2 b9 49 f0 c5 a6 bb e6 26 b7 d9 | \n..t=..I.....&..
        002: 8d 47 d1 87 be 20 0b ea bd a6 5f 84 16 33 81 f7 | .G... ...._..3..
        003: c7 00 da b1 71 dc e2 a1 97 08 4d b7 44 10 61 b3 | ....q.....M.D.a.
        004: 6c c3 c8 14 b2 60 10 37 63 3f f0 0c 70 bb 91 04 | l....`.7c?..p...
        005:                                                 |
        Output (Input State For Debugging):
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 2 13: 9000000 14: 4a000000 15: 0 |}];
      printf
        "Processing the same ciphertext again and observing that we get entirely \
         different output because it is XOR'd with a new state\n";
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
        Processing the same ciphertext again and observing that we get entirely different output because it is XOR'd with a new state
        Start of cycle
        Input:
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 1 13: 9000000 14: 4a000000 15: 0
        Output (Real Output):
        001: dc be bf c8 87 bb 63 81 26 dc c8 05 a2 57 4d 2b | ......c.&....WM+
        002: 0d 7b 80 c9 79 df 8c 02 73 fb 48 8c e9 01 39 07 | \r{..y...s.H...9.
        003: aa a7 3c b0 e8 1f cd 35 b2 5f 73 a0 80 e0 09 6a | ..<....5._s....j
        004: 0a f7 a6 28 7f 7b 28 66 6b 90 c4 d2 61 26 9e 31 | \n..(.{(fk...a&.1
        005:                                                 |
        Output (Input State For Debugging):
         0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
         4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
         8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
         12: 3 13: 9000000 14: 4a000000 15: 0 |}]
    ;;
  end)
;;
