open! Core
open! Hardcaml
open! Signal

(** The module implements a serial poly1305 encoder which computes up to one
    16-byte block per cycle.

    Initially, a user must seed the serial encoder with a key by setting start
    := 1 ; key := a 256 bit key.

    Then, the user must split their input into [< 128] bit blocks and set start
    := 0; input := block; number_of_input_bytes_minus := original block width /
    8 - 1.

    After supplying the final block, the user can read output to get the poly1305
    tag. *)
module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; start : 'a [@bits 1]
    ; key : 'a [@bits 256]
    ; round_input : 'a [@bits 128]
    ; number_of_input_bytes_minus_one : 'a [@bits 4]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { tag : 'a [@bits 128] } [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
