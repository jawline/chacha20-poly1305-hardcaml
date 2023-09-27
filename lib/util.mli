open! Core
open Hardcaml

(** Create a string of length 512 bits (32 bytes) representing a chacha state. *)
val create_state : key:char list -> counter:int -> nonce:char list -> char list

val bytestring_of_bits : Bits.t -> string
val hexdump : string -> unit
val sunscreen_nonce : char list
val block_test_nonce : char list
val ietf_example_initial_state : nonce:char list -> counter:int -> Bits.t
val print_state : Bits.t -> unit
