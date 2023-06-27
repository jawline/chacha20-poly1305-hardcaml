# WIP Chacha20 hardware implementation

This is an implementation of the Chacha20 stream cipher in hardware using the
Hardcaml hardware description language from the IETF standard.

`Chacha20_block` takes an input chacha state (512 bit signal) and outputs a new
512 bit output state as per the block function defined at:
https://datatracker.ietf.org/doc/html/rfc7539#section-2.3.1.
