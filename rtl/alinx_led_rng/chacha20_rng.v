module chacha20_rng (
    reset,
    clear,
    clock,
    chacha20_output
);

    input reset;
    input clear;
    input clock;
    output [511:0] chacha20_output;

    /* signal declarations */
    wire [511:0] _17 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire _15 = 1'b0;
    wire vdd = 1'b1;
    wire _12 = 1'b0;
    wire _11 = 1'b0;
    wire _10 = 1'b1;
    wire _1;
    reg _14;
    wire _16;
    wire _3;
    wire _5;
    wire _7;
    wire [511:0] _19;
    wire [511:0] _8;

    /* logic */
    assign _1 = _10;
    always @(posedge _7 or posedge _3) begin
        if (_3)
            _14 <= _11;
        else
            if (_5)
                _14 <= _12;
            else
                _14 <= _1;
    end
    assign _16 = _14 == _15;
    assign _3 = reset;
    assign _5 = clear;
    assign _7 = clock;
    chacha20_serial_encoder
        serial_encoder
        ( .clock(_7), .clear(_5), .reset(_3), .set_state(_16), .round_input(_17), .round_output(_19[511:0]) );
    assign _8 = _19;

    /* aliases */

    /* output assignments */
    assign chacha20_output = _8;

endmodule
