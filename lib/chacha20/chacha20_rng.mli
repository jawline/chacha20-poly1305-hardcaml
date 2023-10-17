open! Core
open! Hardcaml
open! Signal

(** This module is for testing on a real piece of hardware.

    We seed Chacha20 with a fixed seed and then use it as a
    PRNG to observe it's output in a simulator.

    The stream of 'random' outputs will be fixed for every
    simulation. *)

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; reset : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { chacha20_output : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
