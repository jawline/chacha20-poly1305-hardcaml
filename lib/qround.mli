open! Core
open! Hardcaml
open! Signal

module I : sig
  type 'a t =
    { a : 'a [@bits 32]
    ; b : 'a [@bits 32]
    ; c : 'a [@bits 32]
    ; d : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { a_out : 'a [@bits 32]
    ; b_out : 'a [@bits 32]
    ; c_out : 'a [@bits 32]
    ; d_out : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Signal.t I.t -> Signal.t O.t
