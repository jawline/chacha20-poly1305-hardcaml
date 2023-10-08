open! Core
open! Hardcaml
open! Signal

type 'a t =
  { a : 'a
  ; b : 'a
  ; c : 'a
  ; d : 'a
  }

val qround : Signal.t t -> Signal.t t

module With_chacha20_state : sig
  val qround : which_words:int t -> Signal.t -> Signal.t
end
