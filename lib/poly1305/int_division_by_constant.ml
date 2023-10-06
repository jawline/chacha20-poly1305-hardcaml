open! Core
open! Hardcaml

(** This module computes a multiplier and two shifts that can be used to do an N bit division
    with a fixed divisor using a (2*N) bit width multiplication and two bit shifts.

    Described in: https://dl.acm.org/doi/pdf/10.1145/178243.178249 *)
module Multiplier_and_shifts = struct
  type t =
    { multiplier : Z.t
    ; sh1 : int
    ; sh2 : int
    ; width : int
    }

  let compute ~width ~divisor =
    let l =
      let initial_l = Z.log2 divisor in
      let two_to_l = Z.((of_int 1) lsl initial_l) in
      if Z.(lt two_to_l divisor)
      then initial_l + 1
      else initial_l
    in
    let two_to_l = Z.((of_int 1) lsl l) in
    let two_to_l_minus_divisor = Z.(two_to_l - divisor) in
    let half_of_range = Z.((of_int 1) lsl width) in
    let multiplier = Z.(extract ((half_of_range * two_to_l_minus_divisor / divisor) + of_int 1) 0 width) in
    let sh1 = min l 1 in
    let sh2 = if l = 0 then 0 else 1 in
    { multiplier; sh1; sh2; width }
  ;;

  let evaluate { multiplier; sh1; sh2; width } dividend =
    let open Z in
    let scaled_by_multiplier = extract (dividend * multiplier) 0 Int.(width * 2) in
    let upper_half = extract (scaled_by_multiplier lsl width) 0 width in
    let lower_half = extract ((dividend - upper_half) asr sh1) 0 width in
    (upper_half + lower_half) asr sh2
  ;;

  let%expect_test "Example" =
    let t = compute ~width:128 ~divisor:(Z.of_int 4) in
    let eval i =
      let result = (evaluate t (Z.of_int i)) |>  Z.to_string in
      Core.print_endline result
    in
    eval 0;
    eval 4;
    eval 8;
    eval 50;
    eval 500;
    eval 1000;
    eval 2000;
    [%expect
      {|
      0
      1
      2
      12
      125
      250
      500 |}]
  ;;
end

let divide ~dividend ~divisor =
  let { Multiplier_and_shifts.multiplier; sh1; sh2; width = dividend_width } =
    Multiplier_and_shifts.compute ~divisor ~width:(Signal.width dividend)
  in
  let multiplier = Z.format "%x" multiplier in
  print_s [%message multiplier];
  let multiplier =
    Constant.of_hex_string ~width:dividend_width ~signedness:Unsigned multiplier
    |> Signal.of_constant
  in
  Signal.(
    let scaled_by_multiplier = dividend *: multiplier in
    let upper_half = uresize (srl scaled_by_multiplier dividend_width) dividend_width in
    Signal.(srl (upper_half +: srl (dividend -: upper_half) sh1) sh2))
;;

let%expect_test "divide initialization" =
  let signal = divide ~dividend:(Signal.of_int ~width:130 0) ~divisor:(Z.of_int 150) in
  Core.print_s [%message (signal : Signal.t)];
  ();
  [%expect
    {|
    2d3a06d3a06d3a06d3a06d3a06d3a06d4
    (signal (const (width 130) (value 0x000000000000000000000000000000000))) |}]
;;
