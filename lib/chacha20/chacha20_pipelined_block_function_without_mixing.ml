open! Core
open! Hardcaml
open! Signal

(** An implementation of the Chacha20 block function (10 rounds of the column +
    diagonal qrounds) as described in
    https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1.

    This implementation executes one column and diagonal round per cycle. *)

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; start : 'a [@bits 1]
    ; round_input : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { finished : 'a [@bits 1]
    ; round_output : 'a [@bits 512]
    }
  [@@deriving sexp_of, hardcaml]
end

let create scope ({ clock; clear; start; round_input = first_round_input; _ } : _ I.t) =
  let open Always in
  let open Variable in
  let r_sync = Reg_spec.create ~clock ~clear () in
  let current_state = reg ~enable:vdd ~width:512 r_sync in
  let remaining = reg ~enable:vdd ~width:5 r_sync in
  let round_input = wire ~default:(Signal.of_int ~width:512 0) in
  let { Chacha20_column_and_diagonal_round.O.round_output } =
    Chacha20_column_and_diagonal_round.hierarchical
      ~instance:(Scope.name scope "column_and_diagonal")
      scope
      { Chacha20_column_and_diagonal_round.I.round_input = round_input.value }
  in
  (* Chacha20's block function is 10 column rounds and 10 diagonal rounds back
     to back. As per the pseudocode in the IETF spec we merge the column and
     diagonal rounds into a single circuit
     and then apply it 10 times here. *)
  compile
    [ when_
        (start ==:. 1)
        [ round_input <-- first_round_input
        ; current_state <-- round_output
        ; remaining <--. 9
        ]
    ; when_
        (remaining.value >:. 0)
        [ round_input <-- current_state.value
        ; current_state <-- round_output
        ; remaining <-- remaining.value -:. 1
        ]
    ];
  { O.finished = remaining.value ==:. 0; round_output = current_state.value }
;;

let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical
    ~scope
    ~name:"chacha20_block_function_without_mixing"
    ~instance
    create
    input
;;

module Test_from_ietf = struct
  (* Implements the test vector described in
     https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1 and prints the
     correct output before mixing the old with new. *)

  let cycle_fully ~sim ~(inputs : _ I.t) ~(outputs : _ O.t) =
    printf "Input: \n";
    Util.print_state !(inputs.round_input);
    inputs.start := Bits.of_int ~width:1 1;
    Cyclesim.cycle sim;
    inputs.start := Bits.of_int ~width:1 0;
    printf "Output (First cycle): Finished: %i\n" (Bits.to_int !(outputs.finished));
    Util.print_state !(outputs.round_output);
    Sequence.range 0 8 |> Sequence.iter ~f:(fun _ -> Cyclesim.cycle sim);
    printf "Output (Ninth cycle): Finished: %i\n" (Bits.to_int !(outputs.finished));
    Util.print_state !(outputs.round_output);
    Cyclesim.cycle sim;
    printf "Output (Tenth cycle): Finished: %i\n" (Bits.to_int !(outputs.finished));
    Util.print_state !(outputs.round_output);
    Cyclesim.cycle sim;
    printf "Output (Eleventh cycle): Finished: %i\n" (Bits.to_int !(outputs.finished));
    Util.print_state !(outputs.round_output)
  ;;

  let%expect_test "fixed test input" =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create (Scope.create ~flatten_design:true ())) in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let input = Util.ietf_example_initial_state ~nonce:Util.block_test_nonce ~counter:1 in
    inputs.round_input := input;
    cycle_fully ~sim ~inputs ~outputs;
    [%expect
      {|
      Input:
       00: 61707865 01: 3320646e 02: 79622d32 03: 6b206574
       04: 03020100 05: 07060504 06: 0b0a0908 07: 0f0e0d0c
       08: 13121110 09: 17161514 10: 1b1a1918 11: 1f1e1d1c
       12: 00000001 13: 09000000 14: 4a000000 15: 00000000
      Output (First cycle): Finished: 0
       00: cd52e917 01: 85ab03b4 02: b3457395 03: f96de7dd
       04: c4b7cd22 05: 5c2e187a 06: c95eb461 07: 316c801a
       08: 7bf7d740 09: 7eddd644 10: f1a1bdf5 11: 761246ca
       12: 6b0d58a3 13: 6798471a 14: d737f167 15: f173888d
      Output (Ninth cycle): Finished: 0
       00: b6b87de6 01: 9a016148 02: 93859815 03: 4915e6cc
       04: dc33c122 05: 98e18fa3 06: 5ffbe8fc 07: f7154b8b
       08: 28c1fcb3 09: c4b38f9d 10: 83e2f5d7 11: 7d24b98b
       12: 21078b22 13: b193e7a3 14: b2f49389 15: 6cde6026
      Output (Tenth cycle): Finished: 1
       00: 837778ab 01: e238d763 02: a67ae21e 03: 5950bb2f
       04: c4f2d0c7 05: fc62bb2f 06: 8fa018fc 07: 3f5ec7b7
       08: 335271c2 09: f29489f3 10: eabda8fc 11: 82e46ebd
       12: d19c12b4 13: b04e16de 14: 9e83d0cb 15: 4e3c50a2
      Output (Eleventh cycle): Finished: 1
       00: 837778ab 01: e238d763 02: a67ae21e 03: 5950bb2f
       04: c4f2d0c7 05: fc62bb2f 06: 8fa018fc 07: 3f5ec7b7
       08: 335271c2 09: f29489f3 10: eabda8fc 11: 82e46ebd
       12: d19c12b4 13: b04e16de 14: 9e83d0cb 15: 4e3c50a2 |}]
  ;;
end
