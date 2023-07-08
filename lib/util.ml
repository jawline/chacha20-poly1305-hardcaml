open Core
open Hardcaml

let byte_string_of_int value =
  let open Int in
  let b1 = (value lsr 24) land 0xFF in
  let b2 = (value lsr 16) land 0xFF in
  let b3 = (value lsr 8) land 0xFF in
  let b4 = value land 0xFF in
  [ b1; b2; b3; b4 ] |> List.rev |> List.map ~f:Char.of_int_exn
;;

let create_state ~key ~nonce =
  if List.length key <> 32
  then raise_s [%message "a chacha20 key must be 32 bytes (8 words, 256 bits) long"];
  if List.length nonce <> 12
  then raise_s [%message "a chacha20 nonce must be 12 bytes (3 words, 64 bits) long"];
  let constant_prelude =
    List.concat
      [ byte_string_of_int 0x61707865
      ; byte_string_of_int 0x3320646e
      ; byte_string_of_int 0x79622d32
      ; byte_string_of_int 0x6b206574
      ]
  in
  let counter_prelude = byte_string_of_int 0x1 in
  constant_prelude @ key @ counter_prelude @ nonce
;;

let pad_to ~desired_strlen unpadded_string =
  if String.length unpadded_string < desired_strlen
  then (
    let missing_chars = desired_strlen - String.length unpadded_string in
    String.concat [ unpadded_string; String.init missing_chars ~f:(fun _ -> ' ') ])
  else unpadded_string
;;

let line_to_hex ~line_width (str : string) =
  String.to_list str
  |> List.map ~f:(fun item -> Char.to_int item |> sprintf "%02x")
  |> String.concat ~sep:" "
  |> pad_to ~desired_strlen:((line_width * 3) - 1)
;;

let line_to_escaped_string (str : string) =
  String.to_list str
  |> List.map ~f:(fun (item : char) ->
       let is_newline = Char.(item = '\n') in
       let is_return = Char.(item = '\r') in
       let is_printable = Char.to_int item >= 32 && Char.to_int item <= 126 in
       if is_newline then "\\n" else if is_return then "\\r" else if is_printable then Char.to_string item else ".")
  |> String.concat
;;

let bytestring_of_bits (bits : Bits.t) =
  if Bits.width bits % 8 <> 0
  then raise_s [%message "bytestring_of_bits must be a multiple of 8"];
  String.init
    ~f:(fun i ->
      let lo = i * 8 in
      let hi = lo + 7 in
      Bits.select bits hi lo |> Bits.to_int |> Char.of_int_exn)
    (Bits.width bits / 8)
;;

let hexdump byte_string =
  let len = String.length byte_string in
  let line_width = 16 in
  let rec split_to_lines i =
    let start_idx = i * line_width in
    let end_idx = (i * line_width) + line_width in
    if start_idx <= len
    then (
      let line_len = if end_idx >= len then len - (i * line_width) else line_width in
      let line = String.sub ~pos:(i * line_width) ~len:line_len byte_string in
      line :: split_to_lines (i + 1))
    else []
  in
  let lines = split_to_lines 0 in
  List.iteri
    ~f:(fun index line ->
      printf
        "%03i: %s | %s\n"
        (index + 1)
        (line_to_hex ~line_width line)
        (line_to_escaped_string line))
    lines;
  ()
;;

let%expect_test "hexdump test" =
  hexdump
    "Hello world. This is a really long\x05\x04\x06byte string. Let's see it get split";
  [%expect
    {|
    001: 48 65 6c 6c 6f 20 77 6f 72 6c 64 2e 20 54 68 69 | Hello world. Thi
    002: 73 20 69 73 20 61 20 72 65 61 6c 6c 79 20 6c 6f | s is a really lo
    003: 6e 67 05 04 06 62 79 74 65 20 73 74 72 69 6e 67 | ng...byte string
    004: 2e 20 4c 65 74 27 73 20 73 65 65 20 69 74 20 67 | . Let's see it g
    005: 65 74 20 73 70 6c 69 74                         | et split |}]
;;
