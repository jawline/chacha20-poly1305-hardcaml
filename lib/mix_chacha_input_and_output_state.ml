open! Core
open! Hardcaml
open! Signal

(** This circuit mixes the input and output state together to produce the state' for the next iteration
    as described in: https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1. *)

module I = struct
  type 'a t =
    { input : 'a [@bits 512]
    ; unmixed_output : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create ({ input; unmixed_output } : _ I.t) =
  (* Iterate over each word and sum them (matrix addition). Do not just
     add the signals as the carries would overflow into the next word. *)
  { O.output =
      Sequence.range 0 16
      |> Sequence.map ~f:(fun index ->
        let slot = Util.select_byte_range ~from:(index * 4) ~to_:((index + 1) * 4) in
        slot input +: slot unmixed_output)
      |> Sequence.to_list
      |> concat_lsb
  }
;;

module Test_simple_matrix_addition = struct
  let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
    printf "Start of cycle\n";
    printf "Input: \n";
    Util.print_state !(inputs.input);
    printf "Output: \n";
    Util.print_state !(inputs.unmixed_output);
    Cyclesim.cycle sim;
    printf "Mixed state: \n";
    Util.print_state !(outputs.output)
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
    inputs.input := input_state;
    inputs.unmixed_output := output_state;
    cycle_and_print ~sim ~inputs ~outputs;
    [%expect
      {|
      Start of cycle
      Input:
       00: 00000000 01: 00000001 02: 00000002 03: 00000003
       04: 00000004 05: 00000005 06: 00000006 07: 00000007
       08: 00000008 09: 00000009 10: 0000000a 11: 0000000b
       12: 0000000c 13: 0000000d 14: 0000000e 15: 0000000f
      Output:
       00: 00000000 01: 00000001 02: 00000002 03: 00000003
       04: 00000004 05: 00000005 06: 00000006 07: 00000007
       08: 00000008 09: 00000009 10: 0000000a 11: 0000000b
       12: 0000000c 13: 0000000d 14: 0000000e 15: 0000000f
      Mixed state:
       00: 00000000 01: 00000002 02: 00000004 03: 00000006
       04: 00000008 05: 0000000a 06: 0000000c 07: 0000000e
       08: 00000010 09: 00000012 10: 00000014 11: 00000016
       12: 00000018 13: 0000001a 14: 0000001c 15: 0000001e |}]
  ;;
end
