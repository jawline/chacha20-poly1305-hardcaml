open Core

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
