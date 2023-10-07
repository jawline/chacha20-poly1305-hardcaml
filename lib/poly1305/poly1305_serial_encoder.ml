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
    ; number_of_input_bytes_minus_one : 'a [@bits 4]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output : 'a [@bits 128] } [@@deriving sexp_of, hardcaml]
end

let create
  ({ clock; clear; start; key; input; number_of_input_bytes_minus_one } : Signal.t I.t)
  =
  let open Always in
  let open Variable in
  let r_sync = Reg_spec.create ~clock ~clear () in
  let { Clamp.O.output = r } = Clamp.create { Clamp.I.input = Signal.select key 127 0 } in
  let s = Signal.select key 255 128 in
  let accumulator = reg ~enable:vdd ~width:130 r_sync in
  let { Poly1305_block.O.output = next_accumulator; _ } =
    Poly1305_block.create
      { Poly1305_block.I.input
      ; input_accumulation = accumulator.value
      ; r
      ; number_of_input_bytes_minus_one
      }
  in
  compile
    [ if_ (start ==:. 1) [ accumulator <--. 0 ] [ accumulator <-- next_accumulator ] ];
  { O.output = uresize accumulator.value 128 +: s }
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

    let cycle_and_print ~sim ~(inputs : _ I.t) =
      Util.bytestring_of_bits !(inputs.input) |> Util.hexdump;
      Cyclesim.cycle sim
    ;;

    let%expect_test "fixed test input" =
      let module Simulator = Cyclesim.With_interface (I) (O) in
      let sim = Simulator.create create in
      let inputs : _ I.t = Cyclesim.inputs sim in
      let outputs : _ O.t = Cyclesim.outputs sim in
      inputs.start := Bits.of_int ~width:1 1;
      inputs.key := Util.example_poly1305_key;
      cycle_and_print ~sim ~inputs;
      [%expect
        {|
        001: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
        002:                                                 | |}];
      inputs.start := Bits.of_int ~width:1 0;
      inputs.input := Bits.select example_text_bits 127 0;
      inputs.number_of_input_bytes_minus_one := Bits.of_int ~width:4 15;
      cycle_and_print ~sim ~inputs;
      [%expect
        {|
        001: 43 72 79 70 74 6f 67 72 61 70 68 69 63 20 46 6f | Cryptographic Fo
        002:                                                 | |}];
      inputs.input := Bits.select example_text_bits 255 128;
      inputs.number_of_input_bytes_minus_one := Bits.of_int ~width:4 15;
      cycle_and_print ~sim ~inputs;
      [%expect
        {|
        001: 72 75 6d 20 52 65 73 65 61 72 63 68 20 47 72 6f | rum Research Gro
        002:                                                 | |}];
      inputs.input := Bits.select example_text_bits 383 256;
      inputs.number_of_input_bytes_minus_one := Bits.of_int ~width:4 1;
      cycle_and_print ~sim ~inputs;
      [%expect
        {|
        001: 75 70 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | up..............
        002:                                                 | |}];
      (* We expect the tag: a8:06:1d:c1:30:51:36:c6:c2:2b:8b:af:0c:01:27:a9 *)
      printf "Final tag\n";
      Util.bytestring_of_bits !(outputs.output) |> Util.hexdump;
      [%expect
        {|
        Final tag
        001: a8 06 1d c1 30 51 36 c6 c2 2b 8b af 0c 01 27 a9 | ....0Q6..+....'.
        002:                                                 | |}]
    ;;
  end)
;;
