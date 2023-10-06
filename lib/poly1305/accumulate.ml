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
  type 'a t =
    { padded_input : 'a [@bits 130]
    ; output_accumulation : 'a [@bits 130]
    }
  [@@deriving sexp_of, hardcaml]
end

let p =
  Constant.of_hex_string
    ~width:130
    ~signedness:Unsigned
    "3fffffffffffffffffffffffffffffffb"
  |> Signal.of_constant
;;

(* Since P is sufficiently large that it could only go into accumulation once in the range
   we can do if 'a < a then 'a else a given a' = a - p *)
let accumulation_mod_p accumulation =
  let accumulation' = accumulation -: p in
  mux2 (accumulation' <: accumulation) accumulation' accumulation
;;

let pad_bit = sll (Signal.of_int ~width:130 1) 128

let create ({ input; input_accumulation; r } : _ I.t) =
  (* Algorithm:
     1) pad input block with 1 bit
     2) accumulation = accumulation + padded input
     3) accumulation = r * accumulation (cut to 129 bits)
     4) accumulation = accumulation % p (done with a single subtraction as p is large and would not fit into the range twice). *)
  let padded_input = uresize input 130 |: pad_bit in
  let accumulation = input_accumulation +: padded_input in
  let accumulation = r *: accumulation in
  let accumulation = accumulation_mod_p accumulation in
  { O.output_accumulation = accumulation
  ; padded_input = input_accumulation +: padded_input
  }
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
      "Padhex: 0x%s\n"
      (!(outputs.padded_input)
       |> Bits.to_constant
       |> Constant.to_hex_string ~signedness:Unsigned);
    printf
      "Output: 0x%s\n"
      (!(outputs.output_accumulation)
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
    [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    ("[-:] got inputs of different widths"
      ((mulu (width 258) (arguments (wire add)))
        (const (width 130) (value 0x3fffffffffffffffffffffffffffffffb))))
    Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
    Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
    Called from Hardcaml__Comb.Make.(-:) in file "src/comb.ml", line 488, characters 4-40
    Called from Chacha_hardcaml__Accumulate.accumulation_mod_p in file "lib/poly1305/accumulate.ml", line 33, characters 22-39
    Called from Chacha_hardcaml__Accumulate.create in file "lib/poly1305/accumulate.ml", line 48, characters 21-52
    Called from Hardcaml__Circuit.With_interface.create_exn in file "src/circuit.ml", line 398, characters 18-30
    Called from Hardcaml__Cyclesim.With_interface.create in file "src/cyclesim.ml", line 117, characters 18-81
    Called from Chacha_hardcaml__Accumulate.Functional_test.(fun) in file "lib/poly1305/accumulate.ml", line 92, characters 14-37
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
  ;;
end
