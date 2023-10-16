module chacha20_block_function_without_mixing (
    round_input,
    round_output
);

    input [511:0] round_input;
    output [511:0] round_output;

    /* signal declarations */
    wire [511:0] _2;
    wire [511:0] _15;
    wire [511:0] _3;
    wire [511:0] _17;
    wire [511:0] _4;
    wire [511:0] _19;
    wire [511:0] _5;
    wire [511:0] _21;
    wire [511:0] _6;
    wire [511:0] _23;
    wire [511:0] _7;
    wire [511:0] _25;
    wire [511:0] _8;
    wire [511:0] _27;
    wire [511:0] _9;
    wire [511:0] _29;
    wire [511:0] _10;
    wire [511:0] _31;
    wire [511:0] _11;
    wire [511:0] _33;
    wire [511:0] _12;

    /* logic */
    assign _2 = round_input;
    chacha20_column_and_diagonal_round
        _0
        ( .round_input(_2), .round_output(_15[511:0]) );
    assign _3 = _15;
    chacha20_column_and_diagonal_round
        _1
        ( .round_input(_3), .round_output(_17[511:0]) );
    assign _4 = _17;
    chacha20_column_and_diagonal_round
        _2_0
        ( .round_input(_4), .round_output(_19[511:0]) );
    assign _5 = _19;
    chacha20_column_and_diagonal_round
        _3_0
        ( .round_input(_5), .round_output(_21[511:0]) );
    assign _6 = _21;
    chacha20_column_and_diagonal_round
        _4_0
        ( .round_input(_6), .round_output(_23[511:0]) );
    assign _7 = _23;
    chacha20_column_and_diagonal_round
        _5_0
        ( .round_input(_7), .round_output(_25[511:0]) );
    assign _8 = _25;
    chacha20_column_and_diagonal_round
        _6_0
        ( .round_input(_8), .round_output(_27[511:0]) );
    assign _9 = _27;
    chacha20_column_and_diagonal_round
        _7_0
        ( .round_input(_9), .round_output(_29[511:0]) );
    assign _10 = _29;
    chacha20_column_and_diagonal_round
        _8_0
        ( .round_input(_10), .round_output(_31[511:0]) );
    assign _11 = _31;
    chacha20_column_and_diagonal_round
        _9_0
        ( .round_input(_11), .round_output(_33[511:0]) );
    assign _12 = _33;

    /* aliases */

    /* output assignments */
    assign round_output = _12;

endmodule
