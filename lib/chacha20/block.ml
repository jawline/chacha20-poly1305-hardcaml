open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t = { input : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create ({ input } : _ I.t) =
  let { Unmixed_block.O.output = unmixed_output } =
    Unmixed_block.create { Unmixed_block.I.input }
  in
  let { Mix_input_and_output_state.O.output } =
    Mix_input_and_output_state.create
      { Mix_input_and_output_state.I.input; unmixed_output }
  in
  { O.output }
;;

module Test_from_ietf = struct
  (* Implements the test vector described in
     https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1 and prints the
     correct output after mixing the old state with the new. *)

  let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
    printf "Start of cycle\n";
    printf "Input: \n";
    Util.print_state !(inputs.input);
    Cyclesim.cycle sim;
    printf "Output: \n";
    Util.bytestring_of_bits !(outputs.output) |> Util.hexdump
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    inputs.input
    := Util.ietf_example_initial_state ~counter:1 ~nonce:Util.block_test_nonce;
    cycle_and_print ~sim ~inputs ~outputs;
    (* This test gives the same output as the example serialized block. *)
    [%expect
      {|
      Start of cycle
      Input:
       00: 61707865 01: 3320646e 02: 79622d32 03: 6b206574
       04: 03020100 05: 07060504 06: 0b0a0908 07: 0f0e0d0c
       08: 13121110 09: 17161514 10: 1b1a1918 11: 1f1e1d1c
       12: 00000001 13: 09000000 14: 4a000000 15: 00000000
      Output:
      001: 10 f1 e7 e4 d1 3b 59 15 50 0f dd 1f a3 20 71 c4 | .....;Y.P.... q.
      002: c7 d1 f4 c7 33 c0 68 03 04 22 aa 9a c3 d4 6c 4e | ....3.h.."....lN
      003: d2 82 64 46 07 9f aa 09 14 c2 d7 05 d9 8b 02 a2 | ..dF............
      004: b5 12 9c d1 de 16 4e b9 cb d0 83 e8 a2 50 3c 4e | ......N......P<N
      005:                                                 | |}]
  ;;
end
