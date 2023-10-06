open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; start : 'a [@bits 1]
    ; key : 'a [@bits 256]
    ; input : 'a [@bits 128]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
        type 'a t = { output : 'a [@bits 128] ; output_without_s : 'a [@bits 130] } [@@deriving sexp_of, hardcaml]
end

let create ({ clock; clear; start; key; input } : Signal.t I.t) =
  let open Always in
  let open Variable in
  let r_sync = Reg_spec.create ~clock ~clear () in
  let { Clamp.O.output = r } = Clamp.create { Clamp.I.input = Signal.select key 127 0 } in
  let s = Signal.select key 255 128 in
  let accumulator = reg ~enable:vdd ~width:130 r_sync in
  let { Accumulate.O.output = next_accumulator } =
    Accumulate.create { Accumulate.I.input; input_accumulation = accumulator.value; r }
  in
  compile
    [ if_ (start ==:. 1) [ accumulator <--. 0 ] [ accumulator <-- next_accumulator ] ];
  { O.output = uresize accumulator.value 128 +: s; output_without_s = uresize accumulator.value 130  }
;;

let%test_module "Functional test" =
  (module struct
    let example_text_bits =
      Bits.uresize
        ("Cryptographic Forum Research Group"
         |> String.to_list
         |> List.map ~f:Bits.of_char
         |> Bits.concat_lsb)
        384
    ;;

    let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
      printf "Start of cycle\n";
      Util.bytestring_of_bits !(inputs.input) |> Util.hexdump;
      Cyclesim.cycle sim;
      printf
        "Output: 0x%s\n"
        (!(outputs.output)
         |> Bits.to_constant
         |> Constant.to_hex_string ~signedness:Unsigned);
      printf
        "Output (without application of S): 0x%s\n"
        (!(outputs.output_without_s)
         |> Bits.to_constant
         |> Constant.to_hex_string ~signedness:Unsigned)
    ;;

    let%expect_test "fixed test input" =
      let module Simulator = Cyclesim.With_interface (I) (O) in
      let sim = Simulator.create create in
      let inputs : _ I.t = Cyclesim.inputs sim in
      let outputs : _ O.t = Cyclesim.outputs sim in
      printf "Starting\n";
      inputs.start := Bits.of_int ~width:1 1;
      inputs.key := Util.example_poly1305_key;
      cycle_and_print ~sim ~inputs ~outputs;
      [%expect
        {|
        Starting
        Start of cycle
        001: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
        002:                                                 |
        Output: 0x1bf54941aff6bf4afdb20dfb8a800301
        Output (without application of S): 0x000000000000000000000000000000000 |}];

     inputs.start := Bits.of_int ~width:1 0;
    inputs.input := Bits.select example_text_bits 127 0;
    cycle_and_print ~sim ~inputs  ~outputs;
    [%expect
      {|
        Start of cycle
        001: 43 72 79 70 74 6f 67 72 61 70 68 69 63 20 46 6f | Cryptographic Fo
        002:                                                 |
        Output: 0xe481c0c64d5b6ddc458ff984711c86fd
        Output (without application of S): 0x2c88c77849d64ae9147ddeb88e69c83fc |}];

    inputs.input := Bits.select example_text_bits 255 128;
    cycle_and_print ~sim ~inputs  ~outputs;
    [%expect
      {|
        Start of cycle
        001: 72 75 6d 20 52 65 73 65 61 72 63 68 20 47 72 6f | rum Research Gro
        002:                                                 |
        Output: 0xcc80ce25188ee886b95f184193110346
        Output (without application of S): 0x2b08b84e36898293bbbad0a4608910045 |}];

    inputs.input := Bits.select example_text_bits 383 256;
    cycle_and_print ~sim ~inputs  ~outputs;
    [%expect
      {|
        Start of cycle
        001: 75 70 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | up..............
        002:                                                 |
        Output: 0x08ce4a03cc1e4b173fe5a4c12d947315
        Output (without application of S): 0x0ecd900c21c278bcc423396c5a3147014 |}]
  end)
;;
