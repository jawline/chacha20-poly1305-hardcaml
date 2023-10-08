open! Core
open! Hardcaml
open! Signal

(** An implementation of the qround function as described in pseudocode in the
    IETF specification: https://datatracker.ietf.org/doc/html/rfc7539#section-2 *)

type 'a t =
  { a : 'a
  ; b : 'a
  ; c : 'a
  ; d : 'a
  }

let qround { a; b; c; d } =
  (* The definition in C like notation {|
   1.  a += b; d ^= a; d <<<= 16;
   2.  c += d; b ^= c; b <<<= 12;
   3.  a += b; d ^= a; d <<<= 8;
   4.  c += d; b ^= c; b <<<= 7; |} *)

  (* 1. *)
  let a = a +: b in
  let d = d ^: a in
  let d = rotl d 16 in
  (* 2. *)
  let c = c +: d in
  let b = b ^: c in
  let b = rotl b 12 in
  (* 3. *)
  let a = a +: b in
  let d = d ^: a in
  let d = rotl d 8 in
  (* 4. *)
  let c = c +: d in
  let b = b ^: c in
  let b = rotl b 7 in
  { a; b; c; d }
;;

module With_chacha20_state = struct
  let word_size = 32
  let max_bit = 511
  let word_offset word = (word * word_size) + (word_size - 1), word * word_size

  let select_words_in_state ~(which_words : int t) state =
    let select_word word =
      let hi, lo = word_offset word in
      select state hi lo
    in
    let a = select_word which_words.a in
    let b = select_word which_words.b in
    let c = select_word which_words.c in
    let d = select_word which_words.d in
    { a; b; c; d }
  ;;

  let replace_word_in_state input_state word_index state =
    let hi, lo = word_offset word_index in
    let before = if word_index > 0 then select input_state (lo - 1) 0 else empty in
    let after = if word_index < 15 then select input_state max_bit (hi + 1) else empty in
    concat_lsb_e [ before; state; after ]
  ;;

  let replace_state_with_modified_words
    ~(which_words : int t)
    ~(qround_output : Signal.t t)
    input
    =
    let state' = input in
    let state' = replace_word_in_state state' which_words.a qround_output.a in
    let state' = replace_word_in_state state' which_words.b qround_output.b in
    let state' = replace_word_in_state state' which_words.c qround_output.c in
    let state' = replace_word_in_state state' which_words.d qround_output.d in
    state'
  ;;

  let qround ~(which_words : int t) input =
    let words = select_words_in_state ~which_words input in
    let qround_output = qround words in
    replace_state_with_modified_words ~which_words ~qround_output input
  ;;
end

module Test = struct
  module Test_qround_interface = struct
    module I = struct
      type 'a t =
        { a : 'a [@bits 32]
        ; b : 'a [@bits 32]
        ; c : 'a [@bits 32]
        ; d : 'a [@bits 32]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { a_out : 'a [@bits 32]
        ; b_out : 'a [@bits 32]
        ; c_out : 'a [@bits 32]
        ; d_out : 'a [@bits 32]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create ({ a; b; c; d } : _ I.t) =
      let { a; b; c; d } = qround { a; b; c; d } in
      { O.a_out = a; b_out = b; c_out = c; d_out = d }
    ;;
  end

  let test ~a ~b ~c ~d =
    let open Test_qround_interface in
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    inputs.a := Bits.of_int ~width:32 a;
    inputs.b := Bits.of_int ~width:32 b;
    inputs.c := Bits.of_int ~width:32 c;
    inputs.d := Bits.of_int ~width:32 d;
    Cyclesim.cycle sim;
    ( Bits.to_int !(outputs.a_out)
    , Bits.to_int !(outputs.b_out)
    , Bits.to_int !(outputs.c_out)
    , Bits.to_int !(outputs.d_out) )
  ;;

  let%expect_test "fixed test input" =
    (* Test input from https://datatracker.ietf.org/doc/html/rfc7539#section-2.1.1. Expected input and output are: {|
        For a test vector, we will use the same numbers as in the example,
        adding something random for c.

        o  a = 0x11111111
        o  b = 0x01020304
        o  c = 0x9b8d6f43
        o  d = 0x01234567

        After running a Quarter Round on these four numbers, we get these:

        o  a = 0xea2a92f4
        o  b = 0xcb1cf8ce
        o  c = 0x4581472e
        o  d = 0x5881c4bb
      |} *)
    let a, b, c, d = test ~a:0x11111111 ~b:0x01020304 ~c:0x9b8d6f43 ~d:0x01234567 in
    printf "a: %x, b: %x, c: %x, d: %x\n" a b c d;
    [%expect {|
      a: ea2a92f4, b: cb1cf8ce, c: 4581472e, d: 5881c4bb |}]
  ;;

  module With_chacha20_state = struct
    (* Implementing the following IETF test:

       For a test vector, we will use a ChaCha state that was generated
       randomly:

       Sample ChaCha State

       879531e0  c5ecf37d  516461b1  c9a62f8a
       44c20ef3  3390af7f  d9fc690b  2a5f714c
       53372767  b00a5631  974c541a  359e9963
       5c971061  3d631689  2098d9d6  91dbd320

       We will apply the QUARTERROUND(2,7,8,13) operation to this state.
       For obvious reasons, this one is part of what is called a "diagonal
       round":

       After applying QUARTERROUND(2,7,8,13)

       879531e0  c5ecf37d *bdb886dc  c9a62f8a
       44c20ef3  3390af7f  d9fc690b *cfacafd2
       *e46bea80  b00a5631  974c541a  359e9963
       5c971061 *ccc07c79  2098d9d6  91dbd320

       Note that only the numbers in positions 2, 7, 8, and 13 changed. *)

    module Interface_for_testing = struct
      module I = struct
        type 'a t = { state : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t = { state_out : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
      end

      let create ({ state } : _ I.t) =
        { O.state_out =
            With_chacha20_state.qround ~which_words:{ a = 2; b = 7; c = 8; d = 13 } state
        }
      ;;
    end

    let%expect_test "fixed test input" =
      let open Interface_for_testing in
      let module Simulator = Cyclesim.With_interface (I) (O) in
      let sim = Simulator.create create in
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
      inputs.state := input_state;
      Cyclesim.cycle sim;
      Sequence.range 0 16
      |> Sequence.iter ~f:(fun word ->
        let word_bits = Bits.select !(outputs.state_out) ((word * 32) + 31) (word * 32) in
        printf "%i: %x\n" word (Bits.to_int word_bits));
      [%expect
        {|
      0: 879531e0
      1: c5ecf37d
      2: bdb886dc
      3: c9a62f8a
      4: 44c20ef3
      5: 3390af7f
      6: d9fc690b
      7: cfacafd2
      8: e46bea80
      9: b00a5631
      10: 974c541a
      11: 359e9963
      12: 5c971061
      13: ccc07c79
      14: 2098d9d6
      15: 91dbd320 |}]
    ;;
  end
end
