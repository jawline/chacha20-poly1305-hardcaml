open! Core
open! Hardcaml
open! Signal

module I : sig
  type 'a t = { input : 'a [@bits 128] } [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { output : 'a [@bits 128] } [@@deriving sexp_of, hardcaml]
end

val create : Signal.t I.t -> Signal.t O.t
