module chacha20_serial_encoder_top (
    round_input,
    set_state,
    reset,
    clear,
    clock,
    round_output
);

    input [511:0] round_input;
    input set_state;
    input reset;
    input clear;
    input clock;
    output [511:0] round_output;

    /* signal declarations */
    wire [511:0] _2;
    wire _4;
    wire _6;
    wire _8;
    wire _10;
    wire [511:0] _14;
    wire [511:0] _11;

    /* logic */
    assign _2 = round_input;
    assign _4 = set_state;
    assign _6 = reset;
    assign _8 = clear;
    assign _10 = clock;
    chacha20_serial_encoder
        _0
        ( .clock(_10), .clear(_8), .reset(_6), .set_state(_4), .round_input(_2), .round_output(_14[511:0]) );
    assign _11 = _14;

    /* aliases */

    /* output assignments */
    assign round_output = _11;

endmodule
