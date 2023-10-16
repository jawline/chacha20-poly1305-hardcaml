open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; reset : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { chacha20_output : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create scope ({ clock; clear; reset } : Signal.t I.t) =
  let open Always in
  let open Variable in
  let r_sync = Reg_spec.create ~clock ~clear ~reset () in
  let first_tick = reg ~enable:vdd ~width:1 r_sync in
  let { Chacha20_serial_encoder.O.round_output = chacha20_output } =
    Chacha20_serial_encoder.hierarchical
      ~instance:"0"
      scope
      { Chacha20_serial_encoder.I.clock
      ; clear
      ; reset
      ; set_state = first_tick.value ==:. 0
      ; round_input = Signal.of_int ~width:512 0
      }
  in
  compile [ first_tick <--. 1 ];
  { O.chacha20_output }
;;

let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"chacha20_rng" ~instance:"0" create input
;;
