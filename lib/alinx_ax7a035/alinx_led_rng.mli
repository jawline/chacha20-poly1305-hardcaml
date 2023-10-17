open! Core
open! Hardcaml
open! Signal

(** This module is for testing on a real piece of hardware.

    This implements a LED print out for an ALINX AX7A035 of the Chacha20 state
    once every second (once every 200Mhz). *)

module I : sig
  type 'a t = { clock : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { led1 : 'a [@bits 1]
    ; led2 : 'a [@bits 1]
    ; led3 : 'a [@bits 1]
    ; led4 : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

val create : update_every_n_cycles:int -> Scope.t -> Signal.t I.t -> Signal.t O.t

val hierarchical
  :  update_every_n_cycles:int
  -> instance:string
  -> Scope.t
  -> Signal.t I.t
  -> Signal.t O.t
