open! Core
open! Hardcaml
open! Signal

module States = struct
  type t =
    | Finished
    | Processing
  [@@deriving sexp_of, compare, enumerate]
end

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; start : 'a [@bits 1]
    ; dividend : 'a [@bits 128]
    ; divisor : 'a [@bits 128]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { finished : 'a [@bits 1]
    ; quotient : 'a [@bits 128]
    ; remainder : 'a [@bits 128]
    ; (* TODO: For debugging, remove *) index_register : 'a [@bits 7]
    ; debug_divisor : 'a [@bits 256]
    }
  [@@deriving sexp_of, hardcaml]
end

(* Constructs a bit look up table mux *)
let bit_selector ~input_width ~selector_width bit =
  mux_init
    ~f:(fun bit_index -> Signal.of_int ~width:input_width (1 lsl bit_index))
    bit
    selector_width
;;

let create ({ clock; clear; start; dividend; divisor } : _ I.t) =
  let open Signal in
  let open Always in
  let open Variable in
  let input_width = 128 in
  let selector_width = 7 in
  let r_sync = Reg_spec.create ~clock ~clear () in
  let state = Always.State_machine.create (module States) ~enable:vdd r_sync in
  let finished = Always.Variable.wire ~default:gnd in
  let remainder_register = reg ~enable:vdd ~width:(input_width * 2) r_sync in
  let divisor_register = reg ~enable:vdd ~width:(input_width * 2) r_sync in
  let quotient_register = reg ~enable:vdd ~width:input_width r_sync in
  let index_register = reg ~enable:vdd ~width:7 r_sync in
  (* TODO: We could probably skip a cycle here during start up *)
  let set_registers_and_start =
    [ remainder_register <-- uresize dividend (input_width * 2)
    ; divisor_register <-- sll (uresize divisor (input_width * 2)) input_width
    ; quotient_register <--. 0
    ; index_register <--. input_width - 1
    ; state.set_next Processing
    ; finished <--. 0
    ]
  in
  let single_step_of_restoring_division =
    let remainder_shifted_left = sll remainder_register.value 1 in
    if_
      (remainder_shifted_left >=: divisor_register.value)
      [ quotient_register
        <-- (quotient_register.value
             |: bit_selector ~input_width ~selector_width index_register.value)
      ; remainder_register <-- remainder_shifted_left -: divisor_register.value
      ]
      [ remainder_register <-- remainder_shifted_left ]
  in
  Always.(
    compile
      [ state.switch
          [ Finished, [ if_ start set_registers_and_start [ finished <--. 1 ] ]
          ; ( Processing
            , [ single_step_of_restoring_division
              ; if_
                  (index_register.value ==:. 0)
                  [ finished <--. 1; state.set_next Finished ]
                  [ finished <--. 0; index_register <-- index_register.value -:. 1 ]
              ] )
          ]
      ]);
  { O.finished = finished.value
  ; quotient = select quotient_register.value 127 0
  ; remainder = select remainder_register.value 127 0
  ; index_register = index_register.value
  ; debug_divisor = divisor_register.value
  }
;;

module Functional_test = struct
  let to_int ref = Bits.to_int !ref
  let to_bstr ref = Bits.to_bstr !ref

  let do_division_test ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) dividend divisor =
    inputs.start := Bits.of_int ~width:1 1;
    inputs.dividend := Bits.of_int ~width:128 dividend;
    inputs.divisor := Bits.of_int ~width:128 divisor;
    Cyclesim.cycle sim;
    inputs.start := Bits.of_int ~width:1 0;
    while to_int outputs.finished <> 1 do
      Cyclesim.cycle sim
    done;
    printf
      "%i / %i : Output %i %i\n"
      dividend
      divisor
      (to_int outputs.quotient)
      (to_int outputs.remainder)
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let do_division_test = do_division_test ~sim ~inputs ~outputs in
    do_division_test 5 2;
    [%expect {| 5 / 2 : Output 2 0 |}];
    do_division_test 1024 1;
    [%expect {| 1024 / 1 : Output 2 0 |}];
    do_division_test 32 5;
    [%expect {| 32 / 5 : Output 6 0 |}];
    do_division_test 600 34;
    [%expect {| 600 / 34 : Output 6 0 |}]
  ;;
end
