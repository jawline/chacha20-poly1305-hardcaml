module poly1305_clamp (
    unclamped_r,
    clamped_r
);

    input [127:0] unclamped_r;
    output [127:0] clamped_r;

    /* signal declarations */
    wire [7:0] _28;
    wire [7:0] _27;
    wire [7:0] _26;
    wire [7:0] _24;
    wire [7:0] _25;
    wire [7:0] _22;
    wire [7:0] _23;
    wire [7:0] _21;
    wire [7:0] _20;
    wire [7:0] _18;
    wire [7:0] _19;
    wire [7:0] _16;
    wire [7:0] _17;
    wire [7:0] _15;
    wire [7:0] _14;
    wire [7:0] _12;
    wire [7:0] _13;
    wire [7:0] _10 = 8'b11111100;
    wire [7:0] _9;
    wire [7:0] _11;
    wire [7:0] _8;
    wire [7:0] _7;
    wire [7:0] _5 = 8'b00001111;
    wire [127:0] _2;
    wire [7:0] _4;
    wire [7:0] _6;
    wire [127:0] _29;

    /* logic */
    assign _28 = _2[7:0];
    assign _27 = _2[15:8];
    assign _26 = _2[23:16];
    assign _24 = _2[31:24];
    assign _25 = _24 & _5;
    assign _22 = _2[39:32];
    assign _23 = _22 & _10;
    assign _21 = _2[47:40];
    assign _20 = _2[55:48];
    assign _18 = _2[63:56];
    assign _19 = _18 & _5;
    assign _16 = _2[71:64];
    assign _17 = _16 & _10;
    assign _15 = _2[79:72];
    assign _14 = _2[87:80];
    assign _12 = _2[95:88];
    assign _13 = _12 & _5;
    assign _9 = _2[103:96];
    assign _11 = _9 & _10;
    assign _8 = _2[111:104];
    assign _7 = _2[119:112];
    assign _2 = unclamped_r;
    assign _4 = _2[127:120];
    assign _6 = _4 & _5;
    assign _29 = { _6, _7, _8, _11, _13, _14, _15, _17, _19, _20, _21, _23, _25, _26, _27, _28 };

    /* aliases */

    /* output assignments */
    assign clamped_r = _29;

endmodule
