module poly1305_top (
    number_of_input_bytes_minus_one,
    round_input,
    key,
    start,
    clear,
    clock,
    tag
);

    input [3:0] number_of_input_bytes_minus_one;
    input [127:0] round_input;
    input [255:0] key;
    input start;
    input clear;
    input clock;
    output [127:0] tag;

    /* signal declarations */
    wire [3:0] _2;
    wire [127:0] _4;
    wire [255:0] _6;
    wire _8;
    wire _10;
    wire _12;
    wire [127:0] _16;
    wire [127:0] _13;

    /* logic */
    assign _2 = number_of_input_bytes_minus_one;
    assign _4 = round_input;
    assign _6 = key;
    assign _8 = start;
    assign _10 = clear;
    assign _12 = clock;
    poly1305_serial_encoder
        _0
        ( .clock(_12), .clear(_10), .start(_8), .key(_6), .round_input(_4), .number_of_input_bytes_minus_one(_2), .tag(_16[127:0]) );
    assign _13 = _16;

    /* aliases */

    /* output assignments */
    assign tag = _13;

endmodule
