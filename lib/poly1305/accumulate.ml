open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { input : 'a [@bits 128]
    ; accumulation : 'a [@bits 128]
    ; r : 'a [@bits 128]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { accumulation : 'a [@bits 128] } [@@deriving sexp_of, hardcaml]
end

let create ({ input; accumulation; r } : _ I.t) =
  (* p is a 128 bit literal so unfortunately there is not a nice way
     to represent it other than by bytes *)
  let p =
    [ 0x03
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xff
    ; 0xfb
    ]
    |> List.map ~f:(Signal.of_int ~width:8)
    |> Signal.concat_msb
  in
  let accumulation = accumulation +: input in
  let accumulation = r *: accumulation %: p in
  { O.accumulation }
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
    let sim = Simulator.create create in
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
