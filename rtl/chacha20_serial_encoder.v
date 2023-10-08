module chacha20_serial_encoder (
    round_input,
    set_state,
    clear,
    clock,
    round_output
);

    input [511:0] round_input;
    input set_state;
    input clear;
    input clock;
    output [511:0] round_output;

    /* signal declarations */
    wire [511:0] _2;
    wire _4;
    wire _6;
    wire _8;
    wire [511:0] _12;
    wire [511:0] _9;

    /* logic */
    assign _2 = round_input;
    assign _4 = set_state;
    assign _6 = clear;
    assign _8 = clock;
    chacha20_serial_encoder
        the_one_and_only
        ( .clock(_8), .clear(_6), .set_state(_4), .round_input(_2), .round_output(_12[511:0]) );
    assign _9 = _12;

    /* aliases */

    /* output assignments */
    assign round_output = _9;

endmodule
