# Chacha20 & Poly1305 Hardcaml Implementation

This is an implementation of the Chacha20 stream cipher from the IETF standard
in hardware using Hardcaml.

The Chacha20_serial_encoder and Poly1305_serial_encoder take an input block by
block and compute the new Chacha20 or Poly1305 state in a single cycle.

Precompiled Rtl versions are available in rtl/
