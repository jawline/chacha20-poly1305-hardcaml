open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t = { input_state : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output_state : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create ({ input_state } : _ I.t) =
  let block_output =
    Chacha20_block_no_mixing.create { Chacha20_block_no_mixing.I.input_state }
  in
  let mixed_output =
    Mix_chacha_input_and_output_state.create
      { Mix_chacha_input_and_output_state.I.input_state
      ; output_state = block_output.output_state
      }
  in
  { O.output_state = mixed_output.new_state }
;;

module Test_from_ietf = struct
  (* Implements the test vector described in
     https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1 and prints the
     correct output after mixing the old state with the new. *)
  let print_state bits =
    Sequence.range 0 16
    |> Sequence.iter ~f:(fun word ->
         let word_bits = Bits.select bits ((word * 32) + 31) (word * 32) in
         printf " %i: %x" word (Bits.to_int word_bits);
         if (word + 1) % 4 = 0 then printf "\n";
         ())
  ;;

  let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
    printf "Start of cycle\n";
    printf "Input: \n";
    print_state !(inputs.input_state);
    Cyclesim.cycle sim;
    printf "Output: \n";
    print_state !(outputs.output_state)
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let input_state =
      let example_key =
        let w1 = [ 0x0; 0x01; 0x02; 0x03 ] in
        let w2 = [ 0x04; 0x05; 0x06; 0x07 ] in
        let w3 = [ 0x08; 0x09; 0x0a; 0x0b ] in
        let w4 = [ 0x0c; 0x0d; 0x0e; 0x0f ] in
        let w5 = [ 0x10; 0x11; 0x12; 0x13 ] in
        let w6 = [ 0x14; 0x15; 0x16; 0x17 ] in
        let w7 = [ 0x18; 0x19; 0x1a; 0x1b ] in
        let w8 = [ 0x1c; 0x1d; 0x1e; 0x1f ] in
        w1 @ w2 @ w3 @ w4 @ w5 @ w6 @ w7 @ w8 |> List.map ~f:Char.of_int_exn
      in
      let example_nonce =
        [ 00; 0x00; 0x00; 0x09; 0x00; 0x00; 0x00; 0x4a; 0x00; 0x00; 0x00; 0x00 ]
        |> List.map ~f:Char.of_int_exn
      in
      Util.create_state ~key:example_key ~nonce:example_nonce
      |> List.map ~f:Bits.of_char
      |> Bits.concat_lsb
    in
    inputs.input_state := input_state;
    cycle_and_print ~sim ~inputs ~outputs;
    [%expect
      {|
      Start of cycle
      Input:
       0: 61707865 1: 3320646e 2: 79622d32 3: 6b206574
       4: 3020100 5: 7060504 6: b0a0908 7: f0e0d0c
       8: 13121110 9: 17161514 10: 1b1a1918 11: 1f1e1d1c
       12: 1 13: 9000000 14: 4a000000 15: 0
      Output:
       0: e4e7f110 1: 15593bd1 2: 1fdd0f50 3: c47120a3
       4: c7f4d1c7 5: 368c033 6: 9aaa2204 7: 4e6cd4c3
       8: 466482d2 9: 9aa9f07 10: 5d7c214 11: a2028bd9
       12: d19c12b5 13: b94e16de 14: e883d0cb 15: 4e3c50a2 |}]
  ;;
end
