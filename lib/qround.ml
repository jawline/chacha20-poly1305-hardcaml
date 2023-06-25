open! Core
open! Hardcaml
open! Signal

(** An implementation of the qround function as described in pseudocode in the
    IETF specification: https://datatracker.ietf.org/doc/html/rfc7539#section-2
    *)

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

let create ({ a; b; c; d; _ } : _ I.t) =
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
  { O.a_out = a; b_out = b; c_out = c; d_out = d }
;;

module Test = struct
  let test ~a ~b ~c ~d =
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
    (* TODO: Generate inputs from a verified implementation to compare *)
    let a, b, c, d = test ~a:0x11111111 ~b:0x01020304 ~c:0x9b8d6f43 ~d:0x01234567 in
    printf "a: %x, b: %x, c: %x, d: %x\n" a b c d;
    [%expect {|
      a: ea2a92f4, b: cb1cf8ce, c: 4581472e, d: 5881c4bb |}]
  ;;
end
