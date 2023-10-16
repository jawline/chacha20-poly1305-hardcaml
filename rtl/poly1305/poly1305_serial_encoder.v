module poly1305_serial_encoder (
    clear,
    clock,
    key,
    number_of_input_bytes_minus_one,
    round_input,
    start,
    tag
);

    input clear;
    input clock;
    input [255:0] key;
    input [3:0] number_of_input_bytes_minus_one;
    input [127:0] round_input;
    input start;
    output [127:0] tag;

    /* signal declarations */
    wire [127:0] _31;
    wire vdd = 1'b1;
    wire [129:0] _21 = 130'b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire _2;
    wire [129:0] _20 = 130'b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire _4;
    wire [129:0] _28 = 130'b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [255:0] _6;
    wire [127:0] _17;
    wire [127:0] _19;
    wire [127:0] _7;
    wire [3:0] _9;
    wire [127:0] _11;
    wire [129:0] _25;
    wire [129:0] _12;
    wire _26 = 1'b1;
    wire _14;
    wire _27;
    wire [129:0] _29;
    wire [129:0] _15;
    reg [129:0] _23;
    wire [127:0] _30;
    wire [127:0] _32;

    /* logic */
    assign _31 = _6[255:128];
    assign _2 = clear;
    assign _4 = clock;
    assign _6 = key;
    assign _17 = _6[127:0];
    poly1305_clamp
        poly1305_clamp
        ( .unclamped_r(_17), .clamped_r(_19[127:0]) );
    assign _7 = _19;
    assign _9 = number_of_input_bytes_minus_one;
    assign _11 = round_input;
    poly1305_block
        poly1305_block
        ( .round_input(_11), .number_of_input_bytes_minus_one(_9), .accumulator(_23), .r(_7), .new_accumulator(_25[129:0]) );
    assign _12 = _25;
    assign _14 = start;
    assign _27 = _14 == _26;
    assign _29 = _27 ? _28 : _12;
    assign _15 = _29;
    always @(posedge _4) begin
        if (_2)
            _23 <= _21;
        else
            _23 <= _15;
    end
    assign _30 = _23[127:0];
    assign _32 = _30 + _31;

    /* aliases */

    /* output assignments */
    assign tag = _32;

endmodule
