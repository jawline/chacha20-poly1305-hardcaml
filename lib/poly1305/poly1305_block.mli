open! Core
open! Hardcaml
open! Signal

(** This module implements the accumulate logic without the modulo over P
    which is done in a different step as it requires multi-cycle integer
    division. *)
module I : sig
  type 'a t =
    { round_input : 'a [@bits 128]
    ; number_of_input_bytes_minus_one : 'a [@bits 4]
    ; accumulator : 'a [@bits 130]
    ; r : 'a [@bits 128]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { new_accumulator : 'a [@bits 130] } [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
