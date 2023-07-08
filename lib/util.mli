open! Core

(** Create a string of length 512 bits (32 bytes) representing a chacha state. *)
val create_state : key:char list -> nonce:char list -> char list

val hexdump : string -> unit
