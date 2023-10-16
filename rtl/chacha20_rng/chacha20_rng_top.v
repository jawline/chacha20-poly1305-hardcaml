module chacha20_rng_top (
    reset,
    clear,
    clock,
    chacha20_output
);

    input reset;
    input clear;
    input clock;
    output [511:0] chacha20_output;

    /* signal declarations */
    wire _2;
    wire _4;
    wire _6;
    wire [511:0] _10;
    wire [511:0] _7;

    /* logic */
    assign _2 = reset;
    assign _4 = clear;
    assign _6 = clock;
    chacha20_rng
        _0
        ( .clock(_6), .clear(_4), .reset(_2), .chacha20_output(_10[511:0]) );
    assign _7 = _10;

    /* aliases */

    /* output assignments */
    assign chacha20_output = _7;

endmodule
