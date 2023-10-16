module chacha20_mixing_function (
    unmixed_round_output,
    round_input,
    mixed_output
);

    input [511:0] unmixed_round_output;
    input [511:0] round_input;
    output [511:0] mixed_output;

    /* signal declarations */
    wire [31:0] _52;
    wire [31:0] _51;
    wire [31:0] _53;
    wire [31:0] _49;
    wire [31:0] _48;
    wire [31:0] _50;
    wire [31:0] _46;
    wire [31:0] _45;
    wire [31:0] _47;
    wire [31:0] _43;
    wire [31:0] _42;
    wire [31:0] _44;
    wire [31:0] _40;
    wire [31:0] _39;
    wire [31:0] _41;
    wire [31:0] _37;
    wire [31:0] _36;
    wire [31:0] _38;
    wire [31:0] _34;
    wire [31:0] _33;
    wire [31:0] _35;
    wire [31:0] _31;
    wire [31:0] _30;
    wire [31:0] _32;
    wire [31:0] _28;
    wire [31:0] _27;
    wire [31:0] _29;
    wire [31:0] _25;
    wire [31:0] _24;
    wire [31:0] _26;
    wire [31:0] _22;
    wire [31:0] _21;
    wire [31:0] _23;
    wire [31:0] _19;
    wire [31:0] _18;
    wire [31:0] _20;
    wire [31:0] _16;
    wire [31:0] _15;
    wire [31:0] _17;
    wire [31:0] _13;
    wire [31:0] _12;
    wire [31:0] _14;
    wire [31:0] _10;
    wire [31:0] _9;
    wire [31:0] _11;
    wire [511:0] _2;
    wire [31:0] _7;
    wire [511:0] _4;
    wire [31:0] _6;
    wire [31:0] _8;
    wire [511:0] _54;

    /* logic */
    assign _52 = _2[31:0];
    assign _51 = _4[31:0];
    assign _53 = _51 + _52;
    assign _49 = _2[63:32];
    assign _48 = _4[63:32];
    assign _50 = _48 + _49;
    assign _46 = _2[95:64];
    assign _45 = _4[95:64];
    assign _47 = _45 + _46;
    assign _43 = _2[127:96];
    assign _42 = _4[127:96];
    assign _44 = _42 + _43;
    assign _40 = _2[159:128];
    assign _39 = _4[159:128];
    assign _41 = _39 + _40;
    assign _37 = _2[191:160];
    assign _36 = _4[191:160];
    assign _38 = _36 + _37;
    assign _34 = _2[223:192];
    assign _33 = _4[223:192];
    assign _35 = _33 + _34;
    assign _31 = _2[255:224];
    assign _30 = _4[255:224];
    assign _32 = _30 + _31;
    assign _28 = _2[287:256];
    assign _27 = _4[287:256];
    assign _29 = _27 + _28;
    assign _25 = _2[319:288];
    assign _24 = _4[319:288];
    assign _26 = _24 + _25;
    assign _22 = _2[351:320];
    assign _21 = _4[351:320];
    assign _23 = _21 + _22;
    assign _19 = _2[383:352];
    assign _18 = _4[383:352];
    assign _20 = _18 + _19;
    assign _16 = _2[415:384];
    assign _15 = _4[415:384];
    assign _17 = _15 + _16;
    assign _13 = _2[447:416];
    assign _12 = _4[447:416];
    assign _14 = _12 + _13;
    assign _10 = _2[479:448];
    assign _9 = _4[479:448];
    assign _11 = _9 + _10;
    assign _2 = unmixed_round_output;
    assign _7 = _2[511:480];
    assign _4 = round_input;
    assign _6 = _4[511:480];
    assign _8 = _6 + _7;
    assign _54 = { _8, _11, _14, _17, _20, _23, _26, _29, _32, _35, _38, _41, _44, _47, _50, _53 };

    /* aliases */

    /* output assignments */
    assign mixed_output = _54;

endmodule
