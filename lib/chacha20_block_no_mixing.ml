open! Core
open! Hardcaml
open! Signal

(** An implementation of the Chacha20 block function (10 rounds of the column +
    diagonal qrounds) as described in
    https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1. *)

module I = struct
  type 'a t = { input : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create ({ input; _ } : _ I.t) =
  (* Chacha20's block function is 10 column rounds and 10 diagonal rounds back
     to back. As per the pseudocode in the IETF spec we merge the column and
     diagonal rounds into a single circuit
     and then apply it 10 times here. *)
  { O.output =
      Sequence.range 0 10
      |> Sequence.fold ~init:input ~f:(fun acc _i ->
        let next_round_output =
          Chacha20_column_and_diagonal_round.create
            { Chacha20_column_and_diagonal_round.I.input = acc }
        in
        next_round_output.output)
  }
;;

module Test_informal = struct
  (* This test is just testing that the function simulates without error and is
     not implementing a test from the IETF standard. *)

  let cycle_and_print ~sim ~(outputs : _ O.t) =
    printf "Start of cycle\n";
    Cyclesim.cycle sim;
    Sequence.range 0 16
    |> Sequence.iter ~f:(fun word ->
      let word_bits = Bits.select !(outputs.output) ((word * 32) + 31) (word * 32) in
      printf "%i: %x\n" word (Bits.to_int word_bits))
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let oi v = Bits.of_int ~width:32 v in
    let input =
      [ 0x879531e0
      ; 0xc5ecf37d
      ; 0x516461b1
      ; 0xc9a62f8a
      ; 0x44c20ef3
      ; 0x3390af7f
      ; 0xd9fc690b
      ; 0x2a5f714c
      ; 0x53372767
      ; 0xb00a5631
      ; 0x974c541a
      ; 0x359e9963
      ; 0x5c971061
      ; 0x3d631689
      ; 0x2098d9d6
      ; 0x91dbd320
      ]
      |> List.map ~f:oi
      |> Bits.concat_lsb
    in
    inputs.input := input;
    cycle_and_print ~sim ~outputs;
    [%expect
      {|
      Start of cycle
      0: ca048c1b
      1: 899ba620
      2: e7cc974b
      3: 27e32142
      4: 73548af8
      5: 42363d96
      6: 7f86ac3f
      7: 418e9bdd
      8: a7058054
      9: 479e86c7
      10: e3a44a0
      11: 833f1d8f
      12: 34f829d6
      13: e54005d2
      14: 29a14151
      15: 445b6001 |}];
    inputs.input := !(outputs.output);
    cycle_and_print ~sim ~outputs;
    [%expect
      {|
      Start of cycle
      0: fbca1c95
      1: eefe6a0c
      2: 4d1c93ed
      3: bff50bc1
      4: 601983d1
      5: 463e7a7e
      6: e0f33313
      7: 1fbb227e
      8: 4de07426
      9: c9c8d6de
      10: e1a9a4b1
      11: f0fb2c02
      12: 79ef1dd0
      13: 9318f41b
      14: 70913597
      15: 44d5d95b |}];
    inputs.input := !(outputs.output);
    cycle_and_print ~sim ~outputs;
    [%expect
      {|
      Start of cycle
      0: 2d214396
      1: 5e317630
      2: 165b921e
      3: dcb7fcd9
      4: 78630249
      5: 73ceef7
      6: 79a6092d
      7: 75cf3c1e
      8: d0858917
      9: b4fbc2eb
      10: c10d60b6
      11: 4d154cdd
      12: 397d844e
      13: e64f61b7
      14: a77fed81
      15: 141940a9 |}]
  ;;
end

module Test_from_ietf = struct
  (* Implements the test vector described in
     https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1 and prints the
     correct output before mixing the old with new. *)

  let cycle_and_print ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
    printf "Start of cycle\n";
    printf "Input: \n";
    Util.print_state !(inputs.input);
    Cyclesim.cycle sim;
    printf "Output: \n";
    Util.print_state !(outputs.output)
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let input = Util.ietf_example_initial_state ~nonce:Util.block_test_nonce ~counter:1 in
    inputs.input := input;
    cycle_and_print ~sim ~inputs ~outputs;
    [%expect
      {|
      Start of cycle
      Input:
       00: 61707865 01: 3320646e 02: 79622d32 03: 6b206574
       04: 03020100 05: 07060504 06: 0b0a0908 07: 0f0e0d0c
       08: 13121110 09: 17161514 10: 1b1a1918 11: 1f1e1d1c
       12: 00000001 13: 09000000 14: 4a000000 15: 00000000
      Output:
       00: 837778ab 01: e238d763 02: a67ae21e 03: 5950bb2f
       04: c4f2d0c7 05: fc62bb2f 06: 8fa018fc 07: 3f5ec7b7
       08: 335271c2 09: f29489f3 10: eabda8fc 11: 82e46ebd
       12: d19c12b4 13: b04e16de 14: 9e83d0cb 15: 4e3c50a2 |}]
  ;;
end
