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
  select input_state (word * word_size) ((word * word_size) + (word_size - 1))
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
    if word_index > 0 then select input_state 0 ((word_index * word_size) - 1) else empty
  in
  let after =
    if word_index < 15
    then select input_state ((word_index + 1) * word_size) max_bit
    else empty
  in
  before @: word @: after
;;

let qround_output ~a ~b ~c ~d ~initial_state ~(qround_output : Signal.t Qround.O.t) =
  let state' = initial_state in
  let state' = replace_word state' a qround_output.a_out in
  let state' = replace_word state' b qround_output.b_out in
  let state' = replace_word state' c qround_output.c_out in
  let state' = replace_word state' d qround_output.d_out in
  state'
;;
