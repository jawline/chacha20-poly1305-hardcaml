open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { input : 'a [@bits 128]
    ; input_accumulation : 'a [@bits 130]
    ; r : 'a [@bits 128]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output : 'a [@bits 130] } [@@deriving sexp_of, hardcaml]
end

let p = Z.of_string_base 16 "3fffffffffffffffffffffffffffffffb"
let pad_bit = sll (Signal.of_int ~width:130 1) 128

let create ({ input; input_accumulation; r } : _ I.t) =
  (* Algorithm:
     1) pad input block with 1 bit
     2) accumulation = accumulation + padded input
     3) accumulation = r * accumulation
     4) accumulation = accumulation % p. *)
  let padded_input = uresize input 130 |: pad_bit in
  let accumulation = input_accumulation +: padded_input in
  let accumulation = r *: accumulation in
  let accumulation = Int_division_by_constant.modulo ~dividend:accumulation ~divisor:p in
  { O.output = uresize accumulation 130 }
;;

module Functional_test = struct
  let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
    Cyclesim.cycle sim;
    printf
      "Input: 0x%s\n"
      (!(inputs.input_accumulation)
       |> Bits.to_constant
       |> Constant.to_hex_string ~signedness:Unsigned);
    printf
      "Output: 0x%s\n"
      (!(outputs.output)
       |> Bits.to_constant
       |> Constant.to_hex_string ~signedness:Unsigned)
  ;;

  let ietf_example_r_clamped =
    Constant.of_hex_string
      ~signedness:Unsigned
      ~width:128
      "806d5400e52447c036d555408bed685"
    |> Bits.of_constant
  ;;

  let ietf_example_block =
    Constant.of_hex_string
      ~signedness:Unsigned
      ~width:128
      "6f4620636968706172676f7470797243"
    |> Bits.of_constant
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    inputs.input_accumulation := Bits.of_int ~width:130 0;
    inputs.r := ietf_example_r_clamped;
    inputs.input := ietf_example_block;
    cycle_and_print ~sim ~inputs ~outputs;
    [%expect
      {|
      Input: 0x000000000000000000000000000000000
      Output: 0x2c88c77849d64ae9147ddeb88e69c83fc |}]
  ;;
end
