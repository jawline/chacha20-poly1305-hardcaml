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
      ~instance:(Scope.name scope "serial_encoder")
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

let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"chacha20_rng" ~instance create input
;;

module Test = struct
  (* This is mainly just checking that the create method works and that we
     observe pseudo-random outputs; *)

  let cycle_and_print ~sim ~(outputs : _ O.t) =
    Cyclesim.cycle sim;
    printf "Cycle: \n";
    Util.bytestring_of_bits !(outputs.chacha20_output) |> Util.hexdump
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create (Scope.create ~flatten_design:true ())) in
    let outputs : _ O.t = Cyclesim.outputs sim in
    cycle_and_print ~sim ~outputs;
    [%expect
      {|
      Cycle:
      001: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
      002: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
      003: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
      004: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
      005:                                                 | |}];
    cycle_and_print ~sim ~outputs;
    [%expect
      {|
      Cycle:
      001: 52 3b 05 d3 a0 28 87 f6 7e ef 8b ec 3f 72 3d c2 | R;...(..~...?r=.
      002: c1 77 32 00 d7 79 fa 8d 1f 5f 2a fb d8 4e f5 29 | .w2..y..._*..N.)
      003: 39 88 ab dc 02 64 92 78 7d 27 bd 79 ce ec 5e 4f | 9....d.x}'.y..^O
      004: da 15 f4 fe c0 10 24 7d a9 6d 99 2d 95 5a 54 e9 | ......$}.m.-.ZT.
      005:                                                 | |}];
    cycle_and_print ~sim ~outputs;
    [%expect
      {|
      Cycle:
      001: 64 e6 fa 59 1c 4a 27 7c c2 2c 92 6e 7c fa eb 8c | d..Y.J'|.,.n|...
      002: 83 2e fc 57 62 18 f3 af 59 7e 5d 9c db 44 6e 78 | ...Wb...Y~]..Dnx
      003: 0a 9a 8f 7c ba 55 be 32 cf 1d 4b f6 b3 39 16 b2 | \n..|.U.2..K..9..
      004: 80 87 ff 17 cf 36 f2 49 44 28 fe 02 68 95 f7 30 | .....6.ID(..h..0
      005:                                                 | |}]
  ;;
end
