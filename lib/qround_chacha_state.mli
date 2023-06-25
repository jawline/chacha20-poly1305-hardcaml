open! Core
open! Hardcaml
open! Signal

module Assignment : sig
  type 'a t =
    { a : 'a
    ; b : 'a
    ; c : 'a
    ; d : 'a
    }
end

val select_word : Signal.t -> int -> Signal.t
val qround_inputs : a:int -> b:int -> c:int -> d:int -> Signal.t -> Signal.t Assignment.t

val merge_qround_output
  :  a:int
  -> b:int
  -> c:int
  -> d:int
  -> initial_state:Signal.t
  -> qround_output:Signal.t Qround.O.t
  -> Signal.t
