open! Core
open! Hardcaml
open! Signal

(** This circuit mixes the input and output state together to produce the state' for the next iteration
    as described in: https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1. *)

module I = struct
  type 'a t =
    { input_state : 'a [@bits 512]
    ; output_state : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { new_state : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create ({ input_state; output_state } : _ I.t) =
  (* Iterate over each word and sum them (matrix addition). Do not just
           add the signals as the carries would overflow into the next word. *)
  let new_state =
    Sequence.range 0 16
    |> Sequence.map ~f:(fun index ->
         let lo_bit = index * 32 in
         let hi_bit = lo_bit + 31 in
         select input_state hi_bit lo_bit +: select output_state hi_bit lo_bit)
    |> Sequence.to_list
    |> concat_lsb
  in
  { O.new_state }
;;

module Test_simple_matrix_addition = struct
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
    printf "Output: \n";
    print_state !(inputs.output_state);
    Cyclesim.cycle sim;
    printf "Mixed state: \n";
    print_state !(outputs.new_state)
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let input_state =
      Sequence.range 0 16
      |> Sequence.to_list
      |> List.map ~f:(Bits.of_int ~width:32)
      |> Bits.concat_lsb
    in
    let output_state =
      Sequence.range 0 16
      |> Sequence.to_list
      |> List.map ~f:(Bits.of_int ~width:32)
      |> Bits.concat_lsb
    in
    inputs.input_state := input_state;
    inputs.output_state := output_state;
    cycle_and_print ~sim ~inputs ~outputs;
    [%expect
      {|
      Start of cycle
      Input:
       0: 0 1: 1 2: 2 3: 3
       4: 4 5: 5 6: 6 7: 7
       8: 8 9: 9 10: a 11: b
       12: c 13: d 14: e 15: f
      Output:
       0: 0 1: 1 2: 2 3: 3
       4: 4 5: 5 6: 6 7: 7
       8: 8 9: 9 10: a 11: b
       12: c 13: d 14: e 15: f
      Mixed state:
       0: 0 1: 2 2: 4 3: 6
       4: 8 5: a 6: c 7: e
       8: 10 9: 12 10: 14 11: 16
       12: 18 13: 1a 14: 1c 15: 1e |}]
  ;;
end
