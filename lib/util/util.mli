open! Core
open Hardcaml

(** Create a string of length 512 bits (32 bytes) representing a chacha state. *)
val create_state : key:char list -> counter:int -> nonce:char list -> char list

val bytestring_of_bits : Bits.t -> string
val hexdump : string -> unit
val sunscreen_nonce : char list
val block_test_nonce : char list
val example_clamp_input : Bits.t
val ietf_example_initial_state : nonce:char list -> counter:int -> Bits.t
val print_state : Bits.t -> unit
val select_byte_range : from:int -> to_:int -> Signal.t -> Signal.t
val replace_byte_range : from:int -> to_:int -> with_:Signal.t -> Signal.t -> Signal.t
val hexdump_bits : Bits.t -> unit
