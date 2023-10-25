open! Core
open! Hardcaml
open! Signal

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; set_state : 'a [@bits 1]
    ; start_round : 'a [@bits 1]
    ; round_input : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { finished : 'a [@bits 1]
    ; round_output : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
