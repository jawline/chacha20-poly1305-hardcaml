open! Core
open! Hardcaml
open! Signal

(* The specification at
   https://datatracker.ietf.org/doc/html/rfc7539#section-2.2 specifies indices
   that refer to the inputs and outputs of a qround function.

   The chacha state is 512 bits (16 * 32 u32's). This helper lets us produce a
   circuit from state to qround of state given some fixed indices for a, b, c, and d. *)

let word_size = 32
let max_bit = 511

let select_word input_state word =
  select input_state ((word * word_size) + (word_size - 1)) (word * word_size)
;;

module Assignment = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    ; c : 'a
    ; d : 'a
    }
end

let qround_inputs ~a ~b ~c ~d input_state =
  let select_word = select_word input_state in
  let a = select_word a in
  let b = select_word b in
  let c = select_word c in
  let d = select_word d in
  { Assignment.a; b; c; d }
;;

let replace_word input_state word_index word =
  let before =
    if word_index > 0 then select input_state ((word_index * word_size) - 1) 0 else empty
  in
  let after =
    if word_index < 15
    then select input_state max_bit ((word_index + 1) * word_size)
    else empty
  in
  concat_lsb_e [ before; word; after ]
;;

let merge_qround_output ~a ~b ~c ~d ~initial_state ~(qround_output : Signal.t Qround.O.t) =
  let state' = initial_state in
  let state' = replace_word state' a qround_output.a_out in
  let state' = replace_word state' b qround_output.b_out in
  let state' = replace_word state' c qround_output.c_out in
  let state' = replace_word state' d qround_output.d_out in
  state'
;;

module Test = struct
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

  module I = struct
    type 'a t = { state : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { state_out : 'a [@bits 512]
      ; qround_output : 'a Qround.O.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ({ state; _ } : _ I.t) =
    let slots = qround_inputs ~a:2 ~b:7 ~c:8 ~d:13 state in
    let qround_output =
      Qround.create { Qround.I.a = slots.a; b = slots.b; c = slots.c; d = slots.d }
    in
    let state_out =
      merge_qround_output ~a:2 ~b:7 ~c:8 ~d:13 ~initial_state:state ~qround_output
    in
    { O.state_out; qround_output }
  ;;

  let%expect_test "fixed test input" =
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
         let word_bits =
           Bits.select !(outputs.state_out) ((word * 32) + 31) (word * 32)
         in
         printf "%i: %x\n" word (Bits.to_int word_bits));
    printf
      "Qround output: %x %x %x %x\n"
      (Bits.to_int !(outputs.qround_output.a_out))
      (Bits.to_int !(outputs.qround_output.b_out))
      (Bits.to_int !(outputs.qround_output.c_out))
      (Bits.to_int !(outputs.qround_output.d_out));
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
      15: 91dbd320
      Qround output: bdb886dc cfacafd2 e46bea80 ccc07c79 |}]
  ;;
end
