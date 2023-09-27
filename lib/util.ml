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

let create_state ~key ~counter ~nonce =
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
  let counter_prelude = byte_string_of_int counter in
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
    if is_newline
    then "\\n"
    else if is_return
    then "\\r"
    else if is_printable
    then Char.to_string item
    else ".")
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

let sunscreen_nonce =
  [ 00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x4a; 0x00; 0x00; 0x00; 0x00 ]
  |> List.map ~f:Char.of_int_exn
;;

let block_test_nonce =
  [ 00; 0x00; 0x00; 0x09; 0x00; 0x00; 0x00; 0x4a; 0x00; 0x00; 0x00; 0x00 ]
  |> List.map ~f:Char.of_int_exn
;;

let ietf_example_initial_state ~nonce ~counter =
  let example_key =
    let w1 = [ 0x0; 0x01; 0x02; 0x03 ] in
    let w2 = [ 0x04; 0x05; 0x06; 0x07 ] in
    let w3 = [ 0x08; 0x09; 0x0a; 0x0b ] in
    let w4 = [ 0x0c; 0x0d; 0x0e; 0x0f ] in
    let w5 = [ 0x10; 0x11; 0x12; 0x13 ] in
    let w6 = [ 0x14; 0x15; 0x16; 0x17 ] in
    let w7 = [ 0x18; 0x19; 0x1a; 0x1b ] in
    let w8 = [ 0x1c; 0x1d; 0x1e; 0x1f ] in
    w1 @ w2 @ w3 @ w4 @ w5 @ w6 @ w7 @ w8 |> List.map ~f:Char.of_int_exn
  in
  create_state ~key:example_key ~counter ~nonce
  |> List.map ~f:Bits.of_char
  |> Bits.concat_lsb
;;

let print_state bits =
  Sequence.range 0 16
  |> Sequence.iter ~f:(fun word ->
    let word_bits = Bits.select bits ((word * 32) + 31) (word * 32) in
    printf " %02i: %08x" word (Bits.to_int word_bits);
    if (word + 1) % 4 = 0 then printf "\n";
    ())
;;
