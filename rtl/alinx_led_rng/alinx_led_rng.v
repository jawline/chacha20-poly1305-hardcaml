module alinx_led_rng (
    clock,
    led1,
    led2,
    led3,
    led4
);

    input clock;
    output led1;
    output led2;
    output led3;
    output led4;

    /* signal declarations */
    wire _16;
    wire _17;
    wire _18;
    wire [3:0] _13 = 4'b0000;
    wire [3:0] _11 = 4'b0000;
    wire [511:0] _20;
    wire [511:0] _4;
    wire [3:0] _30;
    wire [27:0] _24 = 28'b1011111010111100000111111111;
    wire vdd = 1'b1;
    wire [27:0] _22 = 28'b0000000000000000000000000000;
    wire _12 = 1'b0;
    wire [27:0] _21 = 28'b0000000000000000000000000000;
    wire _10 = 1'b0;
    wire _6;
    wire [27:0] _28 = 28'b0000000000000000000000000000;
    wire [27:0] _26 = 28'b0000000000000000000000000001;
    wire [27:0] _27;
    wire [27:0] _29;
    wire [27:0] _7;
    reg [27:0] _23;
    wire _25;
    wire [3:0] _31;
    wire [3:0] _8;
    reg [3:0] _15;
    wire _32;

    /* logic */
    assign _16 = _15[3:3];
    assign _17 = _15[2:2];
    assign _18 = _15[1:1];
    chacha20_rng
        rng
        ( .clock(_6), .clear(_12), .reset(_10), .chacha20_output(_20[511:0]) );
    assign _4 = _20;
    assign _30 = _4[3:0];
    assign _6 = clock;
    assign _27 = _23 + _26;
    assign _29 = _25 ? _28 : _27;
    assign _7 = _29;
    always @(posedge _6 or posedge _10) begin
        if (_10)
            _23 <= _21;
        else
            if (_12)
                _23 <= _22;
            else
                _23 <= _7;
    end
    assign _25 = _23 == _24;
    assign _31 = _25 ? _30 : _15;
    assign _8 = _31;
    always @(posedge _6 or posedge _10) begin
        if (_10)
            _15 <= _11;
        else
            if (_12)
                _15 <= _13;
            else
                _15 <= _8;
    end
    assign _32 = _15[0:0];

    /* aliases */

    /* output assignments */
    assign led1 = _32;
    assign led2 = _18;
    assign led3 = _17;
    assign led4 = _16;

endmodule
