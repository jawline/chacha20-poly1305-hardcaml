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
      let two_to_l = Z.(of_int 1 lsl initial_l) in
      if Z.(lt two_to_l divisor) then initial_l + 1 else initial_l
    in
    let two_to_l = Z.(of_int 1 lsl l) in
    let two_to_l_minus_divisor = Z.(two_to_l - divisor) in
    let half_of_range = Z.(of_int 1 lsl width) in
    let multiplier =
      Z.(extract ((half_of_range * two_to_l_minus_divisor / divisor) + of_int 1) 0 width)
    in
    let sh1 = min l 1 in
    let sh2 = if l = 0 then 0 else l - 1 in
    { multiplier; sh1; sh2; width }
  ;;

  let evaluate { multiplier; sh1; sh2; width } dividend =
    let open Z in
    let scaled_by_multiplier = extract (dividend * multiplier) 0 Int.(width * 2) in
    let upper_half = shift_right_trunc scaled_by_multiplier width in
    let lower_half = shift_right_trunc (dividend - upper_half) sh1 in
    let quotient = shift_right_trunc (upper_half + lower_half) sh2 in
    extract quotient 0 width
  ;;

  let%expect_test "Example" =
    let t = compute ~width:128 ~divisor:(Z.of_int 10) in
    print_s
      [%message "" ~multiplier:(Z.to_string t.multiplier) (t.sh1 : int) (t.sh2 : int)];
    [%expect
      {|
      ((multiplier 204169420152563078078024764459060926874) (t.sh1 1) (t.sh2 3)) |}];
    let eval i =
      let result = evaluate t (Z.of_int i) |> Z.to_string in
      print_endline result
    in
    eval 0;
    eval 4;
    eval 8;
    eval 50;
    eval 500;
    eval 1000;
    eval 2000;
    eval 1000000000000000000;
    eval 3183349418695935436;
    [%expect
      {|
      0
      0
      0
      5
      50
      100
      200
      100000000000000000
      318334941869593543 |}]
  ;;

  (* Fuzz test 63 bit ints against OCaml division

      TODO: I could generator Z values instead and test an arbitrary range. *)


  let%test_unit "Fuzzing" =
    Quickcheck.test
      ~sexp_of:[%sexp_of: int]
      (Int.gen_incl 1 Int.max_value)
      ~f:(fun divisor ->
        let t = compute ~width:63 ~divisor:(Z.of_int divisor) in
        Quickcheck.test
          ~sexp_of:[%sexp_of: int]
          (Int.gen_incl 0 Int.max_value)
          ~f:(fun dividend ->
            let evaluated = evaluate t (Z.of_int dividend) |> Z.to_int in
            let locally_evaluated = dividend / divisor in
            [%test_eq: Int.t] evaluated locally_evaluated))
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
    let lower_half = uresize (srl (dividend -: upper_half) sh1) dividend_width in
    Signal.(srl (upper_half +: lower_half) sh2))
;;

let modulo ~dividend ~divisor =
        let dividend_width = Signal.width dividend in
  let quotient = divide ~dividend ~divisor in
  let divisor = Constant.of_hex_string ~width:dividend_width ~signedness:Unsigned (Z.format "%x" divisor ) |> Signal.of_constant in
  let quotient_times_divisor = Signal.(uresize (quotient *: divisor) dividend_width) in
  Signal.(dividend -: quotient_times_divisor)
;;

let%expect_test "divide initialization" =
  let divide = divide ~dividend:(Signal.of_int ~width:130 0) ~divisor:(Z.of_int 150) in
  let modulo = modulo ~dividend:(Signal.of_int ~width:130 0) ~divisor:(Z.of_int 150) in
  Core.print_s [%message (divide : Signal.t) (modulo : Signal.t)];
  [%expect
    {|
    2d3a06d3a06d3a06d3a06d3a06d3a06d4
    2d3a06d3a06d3a06d3a06d3a06d3a06d4
    ((divide (const (width 130) (value 0x000000000000000000000000000000000)))
     (modulo (const (width 130) (value 0x000000000000000000000000000000000)))) |}]
;;
