module chacha20_block (
    round_input,
    start,
    clear,
    clock,
    finished,
    round_output
);

    input [511:0] round_input;
    input start;
    input clear;
    input clock;
    output finished;
    output [511:0] round_output;

    /* signal declarations */
    wire [511:0] _28 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _27 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _18;
    wire [511:0] _20;
    wire [511:0] _1;
    wire _26;
    wire [511:0] _30;
    wire [511:0] _2;
    reg [511:0] _29;
    wire _50 = 1'b1;
    wire vdd = 1'b1;
    wire [1:0] _22 = 2'b00;
    wire [1:0] _21 = 2'b00;
    wire [1:0] _47;
    wire [511:0] _5;
    wire _36 = 1'b1;
    wire _34 = 1'b1;
    wire _7;
    wire _35;
    wire _37;
    wire gnd = 1'b0;
    wire _32;
    wire _38;
    wire _8;
    wire _10;
    wire _12;
    wire [512:0] _17;
    wire _44;
    wire [1:0] _45;
    wire [1:0] _25 = 2'b10;
    wire _42;
    wire [1:0] _43;
    wire [1:0] _40 = 2'b01;
    wire _41;
    wire [1:0] _46;
    wire [1:0] _31 = 2'b00;
    wire _39;
    wire [1:0] _48;
    wire [1:0] _13;
    reg [1:0] _24;
    wire _49;
    wire _51;
    wire _14;

    /* logic */
    assign _18 = _17[512:1];
    chacha20_mixing_function
        mixing
        ( .round_input(_5), .unmixed_round_output(_18), .mixed_output(_20[511:0]) );
    assign _1 = _20;
    assign _26 = _24 == _25;
    assign _30 = _26 ? _1 : _29;
    assign _2 = _30;
    always @(posedge _12) begin
        if (_10)
            _29 <= _28;
        else
            _29 <= _2;
    end
    assign _47 = _35 ? _40 : _24;
    assign _5 = round_input;
    assign _7 = start;
    assign _35 = _7 == _34;
    assign _37 = _35 ? _36 : gnd;
    assign _32 = _24 == _31;
    assign _38 = _32 ? _37 : gnd;
    assign _8 = _38;
    assign _10 = clear;
    assign _12 = clock;
    chacha20_block_function_without_mixing
        pipelined_block
        ( .clock(_12), .clear(_10), .start(_8), .round_input(_5), .round_output(_17[512:1]), .finished(_17[0:0]) );
    assign _44 = _17[0:0];
    assign _45 = _44 ? _25 : _24;
    assign _42 = _24 == _25;
    assign _43 = _42 ? _31 : _24;
    assign _41 = _24 == _40;
    assign _46 = _41 ? _45 : _43;
    assign _39 = _24 == _31;
    assign _48 = _39 ? _47 : _46;
    assign _13 = _48;
    always @(posedge _12) begin
        if (_10)
            _24 <= _22;
        else
            _24 <= _13;
    end
    assign _49 = _24 == _31;
    assign _51 = _49 ? _50 : gnd;
    assign _14 = _51;

    /* aliases */

    /* output assignments */
    assign finished = _14;
    assign round_output = _29;

endmodule
