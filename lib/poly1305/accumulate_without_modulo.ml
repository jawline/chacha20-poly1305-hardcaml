open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { input : 'a [@bits 128]
    ; input_accumulation : 'a [@bits 128]
    ; r : 'a [@bits 128]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output_accumulation : 'a [@bits 128] } [@@deriving sexp_of, hardcaml]
end

let create ({ input; input_accumulation; r } : _ I.t) =
  let accumulation = input_accumulation +: input in
  let accumulation =
    let maybe_overflowed_multiplication = r *: accumulation in
    select maybe_overflowed_multiplication 127 0
  in
  { O.output_accumulation = accumulation }
;;

module Functional_test = struct
  let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
    Cyclesim.cycle sim;
    printf "Input: \n";
    Util.hexdump_bits !(inputs.input_accumulation);
    printf "Output: \n";
    Util.hexdump_bits !(outputs.output_accumulation)
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    inputs.input_accumulation := Bits.of_int ~width:128 0;
    inputs.r := Bits.of_int ~width:128 0xDEADBEEF;
    inputs.input
    := (* TODO jawline: Replace this with a better input for testing *)
       Util.example_clamp_input;
    cycle_and_print ~sim ~inputs ~outputs;
    (* Expectation: 85 ; d6 ; be ; 08 ; 54 ; 55 ; 6d; 03; 7c ; 44 ; 52 ; 0e ; 40 ; d5 ; 06 ; 08

       Source: https://datatracker.ietf.org/doc/html/rfc7539#section-2.5 *)
    [%expect
      {|
      Input:
      001: 85 d6 be 78 57 55 6d 33 7f 44 52 fe 42 d5 06 a8 | ...xWUm3.DR.B...
      002:                                                 |
      Output:
      001: 85 d6 be 08 54 55 6d 03 7c 44 52 0e 40 d5 06 08 | ....TUm.|DR.@...
      002:                                                 | |}]
  ;;
end
