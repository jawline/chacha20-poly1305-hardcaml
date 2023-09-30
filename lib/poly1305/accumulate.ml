open! Core
open! Hardcaml
open! Signal

(* p is a 128 bit literal so unfortunately there is not a nice way
   to represent it other than by bytes *)
let p =
  [ 0x03
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xff
  ; 0xfb
  ]
  |> List.map ~f:(Signal.of_int ~width:8)
  |> Signal.concat_msb
;;
