open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t = { input : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { output : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create ({ input; _ } : _ I.t) =
  let state' = Qround_chacha_state.do_qround ~a:0 ~b:5 ~c:10 ~d:15 input in
  let state' = Qround_chacha_state.do_qround ~a:1 ~b:6 ~c:11 ~d:12 state' in
  let state' = Qround_chacha_state.do_qround ~a:2 ~b:7 ~c:8 ~d:13 state' in
  let state' = Qround_chacha_state.do_qround ~a:3 ~b:4 ~c:9 ~d:14 state' in
  { O.output = state' }
;;

(* Note: Tested in the column and diagonal round ml *)
