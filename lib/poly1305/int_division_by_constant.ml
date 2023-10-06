open! Core
open! Hardcaml

let divide ~dividend ~divisor =
  let l =
    let initial_l = Z.log2 divisor in
    if Z.(leq (shift_left (of_int 1) initial_l) divisor) then initial_l + 1 else initial_l
  in
  let two_to_l = Z.(shift_left (of_int 1) l) in
  let two_to_l_minus_divisor = Z.(two_to_l - divisor) in
  let half_of_range = Z.(shift_left (of_int 1) (Signal.width dividend)) in
  let multiplier = Z.((half_of_range * two_to_l_minus_divisor / divisor) + of_int 1) in
  (* Do I need a multiplier mask? *)
  let sh1 = min l 1 in
  let sh2 = if l = 0 then 0 else 1 in
  let multiplier = Z.format "%x" multiplier in
  print_s [%message multiplier];
  let multiplier =
    Constant.of_hex_string ~width:(Signal.width dividend) ~signedness:Unsigned multiplier
    |> Signal.of_constant
  in
  Signal.(
    let scaled_by_multiplier = dividend *: multiplier in
    let upper_half =
      uresize (srl scaled_by_multiplier (width dividend)) (width dividend)
    in
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
