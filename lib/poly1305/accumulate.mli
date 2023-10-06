open! Core
open! Hardcaml
open! Signal

(** This module implements the accumulate logic without the modulo over P
    which is done in a different step as it requires multi-cycle integer
    division. *)
module I : sig
  type 'a t =
    { input : 'a [@bits 128]
    ; input_accumulation : 'a [@bits 130]
    ; r : 'a [@bits 128]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { output : 'a [@bits 130] } [@@deriving sexp_of, hardcaml]
end

val create : Signal.t I.t -> Signal.t O.t
