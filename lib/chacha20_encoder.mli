open! Core
open! Hardcaml
open! Signal

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; set_state : 'a [@bits 1]
    ; input_state : 'a [@bits 512]
    ; encode : 'a [@bits 1]
    ; encode_data : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { input_state_for_debugging : 'a [@bits 512]
    ; output_state : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end


val create : Signal.t I.t -> Signal.t O.t 
