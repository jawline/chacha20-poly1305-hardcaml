open! Core
open! Hardcaml
open! Signal

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; start : 'a [@bits 1]
    ; key : 'a [@bits 256]
    ; input : 'a [@bits 128]
    ; number_of_input_bytes_minus_one : 'a [@bits 4]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { output : 'a [@bits 128] } [@@deriving sexp_of, hardcaml]
end

val create : Signal.t I.t -> Signal.t O.t
