open! Core
open Hardcaml
open Chacha_hardcaml

let () =
  let poly1305_scope = Scope.create ~flatten_design:true () in
  let chacha20_scope = Scope.create ~flatten_design:true () in
  let poly1305_serial_encoder =
    Circuit.create_with_interface
      ~name:"poly1305_serial_encoder"
      (module Poly1305_serial_encoder.I)
      (module Poly1305_serial_encoder.O)
      (Poly1305_serial_encoder.hierarchical poly1305_scope)
  in
  let chacha20_serial_encoder =
    Circuit.create_with_interface
      ~name:"chacha20_serial_encoder"
      (module Chacha20_serial_encoder.I)
      (module Chacha20_serial_encoder.O)
      (Chacha20_serial_encoder.hierarchical chacha20_scope)
  in
  Rtl.output
    ~database:(Scope.circuit_database poly1305_scope)
    ~output_mode:(In_directory "./rtl/")
    Verilog
    poly1305_serial_encoder;
  Rtl.output
    ~database:(Scope.circuit_database chacha20_scope)
    ~output_mode:(In_directory "./rtl/")
    Verilog
    chacha20_serial_encoder
;;
