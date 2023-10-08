open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t = { input : 'a [@bits 128] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output : 'a [@bits 128] } [@@deriving sexp_of, hardcaml]
end

let create _scope ({ input } : _ I.t) =
  let byte index = Util.select_byte_range ~from:index ~to_:(index + 1) input in
  let l15 = Signal.of_int ~width:8 15 in
  let l252 = Signal.of_int ~width:8 252 in
  let output =
    concat_lsb
      [ byte 0
      ; byte 1
      ; byte 2
      ; byte 3 &: l15
      ; byte 4 &: l252
      ; byte 5
      ; byte 6
      ; byte 7 &: l15
      ; byte 8 &: l252
      ; byte 9
      ; byte 10
      ; byte 11 &: l15
      ; byte 12 &: l252
      ; byte 13
      ; byte 14
      ; byte 15 &: l15
      ]
  in
  { O.output }
;;

let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"poly1305_clamp" ~instance:"the_one_and_only" create input
;;

module Functional_test = struct
  let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
    Cyclesim.cycle sim;
    printf "Input: \n";
    Util.hexdump_bits !(inputs.input);
    printf "Output: \n";
    Util.hexdump_bits !(outputs.output)
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create (Scope.create ~flatten_design:true ())) in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    inputs.input := Util.example_clamp_input;
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
