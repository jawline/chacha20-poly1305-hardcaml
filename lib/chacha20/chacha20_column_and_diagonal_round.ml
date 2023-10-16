open! Core
open! Hardcaml
open! Signal
open! Chacha20_qround.With_chacha20_state

(** An implementation of a single column and a single diagonal round of the Chacha20 block function as described in
    https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.

    ChaCha20 runs 20 rounds, alternating between "column rounds" and
    "diagonal rounds".  Each round consists of four quarter-rounds, and
    they are run as follows.  Quarter rounds 1-4 are part of a "column"
    round, while 5-8 are part of a "diagonal" round:

    1.  QUARTERROUND ( 0, 4, 8,12)
    2.  QUARTERROUND ( 1, 5, 9,13)
    3.  QUARTERROUND ( 2, 6,10,14)
    4.  QUARTERROUND ( 3, 7,11,15)
    5.  QUARTERROUND ( 0, 5,10,15)
    6.  QUARTERROUND ( 1, 6,11,12)
    7.  QUARTERROUND ( 2, 7, 8,13)
    8.  QUARTERROUND ( 3, 4, 9,14)

    At the end of 20 rounds (or 10 iterations of the above list), we add
    the original input words to the output words, and serialize the
    result by sequencing the words one-by-one in little-endian order.

    Note: "addition" in the above paragraph is done modulo 2^32.  In some
    machine languages, this is called carryless addition on a 32-bit
    word. *)

module I = struct
  type 'a t = { round_input : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { round_output : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let column_round round_input =
  let state' = qround ~which_words:{ a = 0; b = 4; c = 8; d = 12 } round_input in
  let state' = qround ~which_words:{ a = 1; b = 5; c = 9; d = 13 } state' in
  let state' = qround ~which_words:{ a = 2; b = 6; c = 10; d = 14 } state' in
  let state' = qround ~which_words:{ a = 3; b = 7; c = 11; d = 15 } state' in
  state'
;;

let diagonal_round round_input =
  let state' = qround ~which_words:{ a = 0; b = 5; c = 10; d = 15 } round_input in
  let state' = qround ~which_words:{ a = 1; b = 6; c = 11; d = 12 } state' in
  let state' = qround ~which_words:{ a = 2; b = 7; c = 8; d = 13 } state' in
  let state' = qround ~which_words:{ a = 3; b = 4; c = 9; d = 14 } state' in
  state'
;;

let create _scope ({ round_input; _ } : _ I.t) =
  let column_output = column_round round_input in
  let round_output = diagonal_round column_output in
  { O.round_output }
;;

let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"chacha20_column_and_diagonal_round" ~instance create input
;;

module Test = struct
  (* This test is just testing that the function simulates without error and is
     not implementing a test from the IETF standard. *)

  let cycle_and_print ~sim ~(outputs : _ O.t) =
    printf "Start of cycle\n";
    Cyclesim.cycle sim;
    Sequence.range 0 16
    |> Sequence.iter ~f:(fun word ->
      let word_bits =
        Bits.select !(outputs.round_output) ((word * 32) + 31) (word * 32)
      in
      printf "%i: %x\n" word (Bits.to_int word_bits))
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create (Scope.create ~flatten_design:true ())) in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let oi v = Bits.of_int ~width:32 v in
    let input_state =
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
    inputs.round_input := input_state;
    cycle_and_print ~sim ~outputs;
    [%expect
      {|
      Start of cycle
      0: ca00c66d
      1: 275a7912
      2: 748d59dc
      3: 4851de9b
      4: f85cf8e2
      5: 4dd5348a
      6: 3da082b
      7: e1194d01
      8: 3d2db76
      9: 2cc126c8
      10: f6ec5a4e
      11: 1f8e5524
      12: 53d2dd35
      13: cd88cc68
      14: eb19d17f
      15: 9931f9d0 |}];
    inputs.round_input := !(outputs.round_output);
    cycle_and_print ~sim ~outputs;
    [%expect
      {|
      Start of cycle
      0: 2756d3c0
      1: 739b230c
      2: e9a144
      3: 67d1631f
      4: a46a63a2
      5: 70443c07
      6: 40308042
      7: 7ea9c8df
      8: a4e26849
      9: fb421a
      10: 51f56ec5
      11: 7ed49af6
      12: 349ce2eb
      13: 675d3f10
      14: 46fc4d77
      15: caa42c38 |}];
    inputs.round_input := !(outputs.round_output);
    cycle_and_print ~sim ~outputs;
    [%expect
      {|
      Start of cycle
      0: d49b3458
      1: 1eb53431
      2: 7ded9a17
      3: af628dd0
      4: d8a7aca1
      5: 616d42a5
      6: 37291cde
      7: a0f257a5
      8: d75e410d
      9: 7203dc7
      10: c1f2b671
      11: 150c3cd1
      12: 5c919c4
      13: 6bf14e15
      14: 8c9ec721
      15: efd0c54d |}]
  ;;
end
