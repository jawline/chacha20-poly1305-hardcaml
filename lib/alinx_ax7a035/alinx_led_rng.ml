open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t = { (* 200Mhz clock source *) clock : 'a [@bits 1] }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { led1 : 'a [@bits 1]
    ; led2 : 'a [@bits 1]
    ; led3 : 'a [@bits 1]
    ; led4 : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

let create scope ({ clock } : Signal.t I.t) =
  let open Always in
  let open Variable in
  let clear = Signal.of_int ~width:1 0 in
  let reset = Signal.of_int ~width:1 0 in
  let r_sync = Reg_spec.create ~clock ~clear ~reset () in
  let counter = reg ~enable:vdd ~width:28 r_sync in
  let leds = reg ~enable:vdd ~width:4 r_sync in
  let { Chacha20_rng.O.chacha20_output } =
    Chacha20_rng.hierarchical
      ~instance:(Scope.name scope "rng")
      scope
      { Chacha20_rng.I.clock; clear; reset }
  in
  compile
    [ counter <-- counter.value +:. 1
    ; if_
        (counter.value ==:. 200_000_000)
        [ leds <-- Signal.select chacha20_output 3 0 ]
        []
    ];
  { O.led1 = Signal.select leds.value 0 0
  ; led2 = Signal.select leds.value 1 1
  ; led3 = Signal.select leds.value 2 2
  ; led4 = Signal.select leds.value 3 3
  }
;;

let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"alinx_led_rng" ~instance create input
;;

(* TODO: Add some sanity tests *)
