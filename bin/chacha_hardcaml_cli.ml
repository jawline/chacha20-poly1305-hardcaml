open! Core
open Hardcaml
open Chacha_hardcaml

let () =
  let circuit =
    Circuit.create_with_interface
      ~name:"chacha20_serial_encoder"
      (module Chacha20_serial_encoder.I)
      (module Chacha20_serial_encoder.O)
      (Chacha20_serial_encoder.create (Scope.create ()))
  in
  Rtl.print Verilog circuit
;;
