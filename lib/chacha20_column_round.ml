open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t = { input_state : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { state_out : 'a [@bits 512] } [@@deriving sexp_of, hardcaml]
end

let create ({ input_state; _ } : _ I.t) =
  let state' = input_state in
  let state' = Qround_chacha_state.do_qround ~a:0 ~b:4 ~c:8 ~d:12 state' in
  let state' = Qround_chacha_state.do_qround ~a:1 ~b:5 ~c:9 ~d:13 state' in
  let state' = Qround_chacha_state.do_qround ~a:2 ~b:6 ~c:10 ~d:14 state' in
  let state' = Qround_chacha_state.do_qround ~a:3 ~b:7 ~c:11 ~d:15 state' in
  { O.state_out = state' }
;;

(* Note: Tested in the column and diagonal round ml *)
