# Chacha20 & Poly1305 Hardcaml Implementation

This is an implementation of the Chacha20 stream cipher from the IETF standard
in hardware using Hardcaml.

The Chacha20_serial_encoder and Poly1305_serial_encoder take an input block by
block and compute the new Chacha20 or Poly1305 state in a single cycle. If a
user wants to manage their state directly, the Chacha20_block and
Poly1305_block modules can be used instead. These take inputs for the previous
block state and the new input and compute the next state.

Precompiled Rtl versions are available in rtl/
