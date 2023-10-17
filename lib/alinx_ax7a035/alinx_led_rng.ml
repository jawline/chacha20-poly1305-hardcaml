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

let create ~update_every_n_cycles scope ({ clock } : Signal.t I.t) =
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
        (counter.value ==:. update_every_n_cycles - 1)
        [ leds <-- Signal.select chacha20_output 3 0; counter <--. 0 ]
        []
    ];
  { O.led1 = Signal.select leds.value 0 0
  ; led2 = Signal.select leds.value 1 1
  ; led3 = Signal.select leds.value 2 2
  ; led4 = Signal.select leds.value 3 3
  }
;;

let hierarchical ~update_every_n_cycles ~instance (scope : Scope.t) (input : Signal.t I.t)
  =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical
    ~scope
    ~name:"alinx_led_rng"
    ~instance
    (create ~update_every_n_cycles)
    input
;;

module Test = struct
  let cycle_and_print ~sim ~(outputs : _ O.t) =
    (* The test will configure a circuit to update it's state every 4
       cycles to the PRNG output so we cycle 4 times between each print,
       leading to two prints per update.

       One lets us observe change and a second lets us check it's
       consistent. *)
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    let value v = Bits.to_int !v in
    printf
      "Cycle: %i %i %i %i\n"
      (value outputs.led1)
      (value outputs.led2)
      (value outputs.led3)
      (value outputs.led4)
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim =
      Simulator.create
        (create ~update_every_n_cycles:4 (Scope.create ~flatten_design:true ()))
    in
    let outputs : _ O.t = Cyclesim.outputs sim in
    (* Cycle once for start up *)
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 0 0 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 0 0 1 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 0 0 1 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 1 1 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 1 1 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 0 0 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 0 0 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 1 1 1 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 1 1 1 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 1 1 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 1 1 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 1 0 0 1 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {|
      Cycle: 1 0 0 1 |}]
  ;;
end
