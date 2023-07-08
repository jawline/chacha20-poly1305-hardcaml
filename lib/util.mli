open! Core
open Hardcaml

(** Create a string of length 512 bits (32 bytes) representing a chacha state. *)
val create_state : key:char list -> nonce:char list -> char list

val bytestring_of_bits : Bits.t -> string
val hexdump : string -> unit
