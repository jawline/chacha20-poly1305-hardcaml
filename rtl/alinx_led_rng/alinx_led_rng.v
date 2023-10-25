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
    wire [511:0] _40 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011;
    wire _38 = 1'b1;
    wire _39;
    wire _36 = 1'b0;
    wire _34 = 1'b0;
    wire _33 = 1'b0;
    wire _19 = 1'b1;
    wire _4;
    reg _35;
    wire _37;
    wire [512:0] _42;
    wire [511:0] _43;
    wire [3:0] _44;
    wire _31 = 1'b1;
    wire [31:0] _29 = 32'b00000000000000000000000000000101;
    wire vdd = 1'b1;
    wire [31:0] _21 = 32'b00000000000000000000000000000000;
    wire _12 = 1'b0;
    wire [31:0] _20 = 32'b00000000000000000000000000000000;
    wire _10 = 1'b0;
    wire _6;
    wire [31:0] _27 = 32'b00000000000000000000000000000000;
    wire [31:0] _25 = 32'b00000000000000000000000000000001;
    wire [31:0] _26;
    wire [31:0] _23 = 32'b00000000000000000000000000000101;
    wire _24;
    wire [31:0] _28;
    wire [31:0] _7;
    reg [31:0] _22;
    wire _30;
    wire _32;
    wire [3:0] _45;
    wire [3:0] _8;
    reg [3:0] _15;
    wire _46;

    /* logic */
    assign _16 = _15[3:3];
    assign _17 = _15[2:2];
    assign _18 = _15[1:1];
    assign _39 = _35 == _38;
    assign _4 = _19;
    always @(posedge _6 or posedge _10) begin
        if (_10)
            _35 <= _33;
        else
            if (_12)
                _35 <= _34;
            else
                _35 <= _4;
    end
    assign _37 = _35 == _36;
    chacha20_serial_encoder
        rng
        ( .clock(_6), .clear(_12), .set_state(_37), .start_round(_39), .round_input(_40), .round_output(_42[512:1]), .finished(_42[0:0]) );
    assign _43 = _42[512:1];
    assign _44 = _43[3:0];
    assign _6 = clock;
    assign _26 = _22 + _25;
    assign _24 = _22 == _23;
    assign _28 = _24 ? _27 : _26;
    assign _7 = _28;
    always @(posedge _6 or posedge _10) begin
        if (_10)
            _22 <= _20;
        else
            if (_12)
                _22 <= _21;
            else
                _22 <= _7;
    end
    assign _30 = _22 == _29;
    assign _32 = _30 == _31;
    assign _45 = _32 ? _44 : _15;
    assign _8 = _45;
    always @(posedge _6 or posedge _10) begin
        if (_10)
            _15 <= _11;
        else
            if (_12)
                _15 <= _13;
            else
                _15 <= _8;
    end
    assign _46 = _15[0:0];

    /* aliases */

    /* output assignments */
    assign led1 = _46;
    assign led2 = _18;
    assign led3 = _17;
    assign led4 = _16;

endmodule
