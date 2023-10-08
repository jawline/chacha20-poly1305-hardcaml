open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { input : 'a [@bits 128]
    ; number_of_input_bytes_minus_one : 'a [@bits 4]
    ; input_accumulation : 'a [@bits 130]
    ; r : 'a [@bits 128]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output : 'a [@bits 130] } [@@deriving sexp_of, hardcaml]
end

let p = Z.of_string_base 16 "3fffffffffffffffffffffffffffffffb"

(** In the accumulate Poly1305 step we need to pad the number of octets
    provided with a leading 1 bit.

    E.g, if a single byte is provided 0b1111_1111 then we should pad it to
    0b1_1111_1111 but if all 16 are provided then we should pad it with a 129th
    bit. We do this by ORing the input with the shifted bit, where the shifted
    bit is computed by muxing the input number of bytes minus one (so that it
    fits in the range of 4 bits, 0..15). *)
let pad_input input number_of_input_bytes_minus_one =
  let shift_bits index = (index + 1) * 8 in
  let table_of_shifted_bits =
    (List.init ~f:(fun index -> sll (Signal.of_int ~width:130 1) (shift_bits index))) 16
  in
  let pad_bit = mux number_of_input_bytes_minus_one table_of_shifted_bits in
  let input = uresize input 130 in
  input |: pad_bit
;;

let create
  _scope
  ({ input; input_accumulation; number_of_input_bytes_minus_one; r } : _ I.t)
  =
  (* Algorithm:
     1) pad input block with 1 bit
     2) accumulation = accumulation + padded input
     3) accumulation = r * accumulation
     4) accumulation = accumulation % p. *)
  let padded_input = pad_input input number_of_input_bytes_minus_one in
  let accumulation = uresize input_accumulation 131 +: uresize padded_input 131 in
  let accumulation_mul = r *: accumulation in
  let accumulation =
    Int_division_by_constant.modulo ~dividend:accumulation_mul ~divisor:p
  in
  { O.output = uresize accumulation 130 }
;;

let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"poly1305_block" ~instance:"the_one_and_only" create input
;;

module Functional_test = struct
  let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
    Cyclesim.cycle sim;
    printf
      "Accumulator: 0x%s Block: 0x%s R: 0x%s\n"
      (!(inputs.input_accumulation)
       |> Bits.to_constant
       |> Constant.to_hex_string ~signedness:Unsigned)
      (!(inputs.input) |> Bits.to_constant |> Constant.to_hex_string ~signedness:Unsigned)
      (!(inputs.r) |> Bits.to_constant |> Constant.to_hex_string ~signedness:Unsigned);
    printf
      "Output: 0x%s\n"
      (!(outputs.output)
       |> Bits.to_constant
       |> Constant.to_hex_string ~signedness:Unsigned)
  ;;

  let of_hex inp = Bits.of_hex ~signedness:Unsigned ~width:128 inp
  let ietf_example_r_clamped = of_hex "806d5400e52447c036d555408bed685"
  let ietf_example_block_zero = of_hex "6f4620636968706172676f7470797243"
  let ietf_example_block_one = of_hex "6f7247206863726165736552206d7572"
  let ietf_example_block_two = of_hex "7075"

  let%expect_test "IETF example test block one" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create (Scope.create ~flatten_design:true)) in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    inputs.input_accumulation := Bits.of_int ~width:130 0;
    inputs.r := ietf_example_r_clamped;
    inputs.input := ietf_example_block_zero;
    inputs.number_of_input_bytes_minus_one := Bits.of_int ~width:4 15;
    cycle_and_print ~sim ~inputs ~outputs;
    [%expect
      {|
      Accumulator: 0x000000000000000000000000000000000 Block: 0x6f4620636968706172676f7470797243 R: 0x0806d5400e52447c036d555408bed685
      Output: 0x2c88c77849d64ae9147ddeb88e69c83fc |}];
    inputs.input_accumulation := !(outputs.output);
    inputs.input := ietf_example_block_one;
    inputs.number_of_input_bytes_minus_one := Bits.of_int ~width:4 15;
    cycle_and_print ~sim ~inputs ~outputs;
    [%expect
      {|
      Accumulator: 0x2c88c77849d64ae9147ddeb88e69c83fc Block: 0x6f7247206863726165736552206d7572 R: 0x0806d5400e52447c036d555408bed685
      Output: 0x2d8adaf23b0337fa7cccfb4ea344b30de |}];
    inputs.input_accumulation := !(outputs.output);
    inputs.input := ietf_example_block_two;
    inputs.number_of_input_bytes_minus_one := Bits.of_int ~width:4 1;
    cycle_and_print ~sim ~inputs ~outputs;
    [%expect
      {|
      Accumulator: 0x2d8adaf23b0337fa7cccfb4ea344b30de Block: 0x00000000000000000000000000007075 R: 0x0806d5400e52447c036d555408bed685
      Output: 0x28d31b7caff946c77c8844335369d03a7 |}]
  ;;
end
