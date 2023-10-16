module chacha20_block (
    round_input,
    round_output
);

    input [511:0] round_input;
    output [511:0] round_output;

    /* signal declarations */
    wire [511:0] _7;
    wire [511:0] _1;
    wire [511:0] _3;
    wire [511:0] _9;
    wire [511:0] _4;

    /* logic */
    chacha20_block_function_without_mixing
        _0
        ( .round_input(_3), .round_output(_7[511:0]) );
    assign _1 = _7;
    assign _3 = round_input;
    chacha20_mixing_function
        _0_0
        ( .round_input(_3), .unmixed_round_output(_1), .mixed_output(_9[511:0]) );
    assign _4 = _9;

    /* aliases */

    /* output assignments */
    assign round_output = _4;

endmodule
