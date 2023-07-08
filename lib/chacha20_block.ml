open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t = { input_state : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output_state : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create ({ input_state } : _ I.t) =
  let block_output =
    Chacha20_block_no_mixing.create { Chacha20_block_no_mixing.I.input_state }
  in
  let mixed_output =
    Mix_chacha_input_and_output_state.create
      { Mix_chacha_input_and_output_state.I.input_state
      ; output_state = block_output.output_state
      }
  in
  { O.output_state = mixed_output.new_state }
;;

module Test_from_ietf = struct
  (* Implements the test vector described in
     https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1 and prints the
     correct output after mixing the old state with the new. *)
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
    printf "Output: \n";
    print_state !(outputs.output_state)
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let input_state = Util.ietf_example_initial_state
    in
    inputs.input_state := input_state;
    cycle_and_print ~sim ~inputs ~outputs;
    [%expect
      {|
      Start of cycle
      Input:
       0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
       4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
       8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
       12: 1 13: 9000000 14: 4a000000 15: 0
      Output:
       0: e4e7f110 1: 15593bd1 2: 1fdd0f50 3: c47120a3
       4: c7f4d1c7 5: 368c033 6: 9aaa2204 7: 4e6cd4c3
       8: 466482d2 9: 9aa9f07 10: 5d7c214 11: a2028bd9
       12: d19c12b5 13: b94e16de 14: e883d0cb 15: 4e3c50a2 |}]
  ;;
end
