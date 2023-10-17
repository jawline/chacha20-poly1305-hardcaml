open! Core
open Hardcaml
open Chacha_hardcaml

let emit
  (type i o)
  ~name
  ~directory
  (module I : Interface.S_Of_signal with type Of_signal.t = i)
  (module O : Interface.S_Of_signal with type Of_signal.t = o)
  create
  =
  printf "Emitting %s\n" name;
  Core_unix.mkdir_p directory;
  let scope = Scope.create ~flatten_design:false () in
  let circuit =
    Circuit.create_with_interface ~name (module I) (module O) (create scope)
  in
  Rtl.output
    ~database:(Scope.circuit_database scope)
    ~output_mode:(In_directory directory)
    Verilog
    circuit
;;

let () =
  emit
    ~name:"poly1305_top"
    ~directory:"./rtl/poly1305/"
    (module Poly1305_serial_encoder.I)
    (module Poly1305_serial_encoder.O)
    (Poly1305_serial_encoder.hierarchical ~instance:"0");
  emit
    ~name:"chacha20_rng_top"
    ~directory:"./rtl/chacha20_rng/"
    (module Chacha20_rng.I)
    (module Chacha20_rng.O)
    (Chacha20_rng.hierarchical ~instance:"0");
  emit
    ~name:"chacha20_serial_encoder_top"
    ~directory:"./rtl/chacha20_serial_encoder/"
    (module Chacha20_serial_encoder.I)
    (module Chacha20_serial_encoder.O)
    (Chacha20_serial_encoder.hierarchical ~instance:"0")
;;
