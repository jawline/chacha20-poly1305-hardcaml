open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t = { clock : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
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

let pipeline_counter r_sync ~update_every_n_cycles =
  let open Always in
  let open Variable in
  let counter = reg ~enable:vdd ~width:32 r_sync in
  compile [ counter <-- mod_counter ~max:update_every_n_cycles counter.value ];
  counter.value ==:. update_every_n_cycles
;;

let create ~update_every_n_cycles scope ({ clock } : Signal.t I.t) =
  let open Always in
  let open Variable in
  let clear = Signal.of_int ~width:1 0 in
  let reset = Signal.of_int ~width:1 0 in
  let r_sync = Reg_spec.create ~clock ~clear ~reset () in
  let first_cycle = reg ~enable:vdd ~width:1 r_sync in
  let leds = reg ~enable:vdd ~width:4 r_sync in
  let { Chacha20_pipelined_serial_encoder.O.round_output = chacha20_output; finished = _ }
    =
    Chacha20_pipelined_serial_encoder.hierarchical
      ~instance:(Scope.name scope "rng")
      scope
      { Chacha20_pipelined_serial_encoder.I.clock
      ; clear
      ; set_state = first_cycle.value ==:. 0
      ; start_round = first_cycle.value ==:. 1
      ; round_input = Signal.of_int ~width:512 3
      }
  in
  let flush = pipeline_counter ~update_every_n_cycles r_sync in
  compile
    [ first_cycle <--. 1
    ; when_ (flush ==:. 1) [ leds <-- Signal.select chacha20_output 3 0 ]
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
    (* The test will configure a circuit to update it's state every 34
       cycles to the PRNG output so we cycle 34 times between each print,
       leading to two prints per update.

       One lets us observe change and a second lets us check it's
       consistent. *)
    Sequence.range 0 32 |> Sequence.iter ~f:(fun _ -> Cyclesim.cycle sim);
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
        (create ~update_every_n_cycles:32 (Scope.create ~flatten_design:true ()))
    in
    let outputs : _ O.t = Cyclesim.outputs sim in
    (* Cycle once for start up *)
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 0 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 1 1 1 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 1 0 1 1 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 1 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 1 1 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 1 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 0 0 0 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 1 0 0 1 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 0 0 1 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 0 0 1 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 1 1 1 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 1 1 1 1 |}];
    cycle_and_print ~sim ~outputs;
    [%expect {| Cycle: 0 1 1 0 |}]
  ;;
end
