module alinx_led_rng_top (
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
    wire _9;
    wire _10;
    wire _11;
    wire _5;
    wire [3:0] _8;
    wire _12;

    /* logic */
    assign _9 = _8[3:3];
    assign _10 = _8[2:2];
    assign _11 = _8[1:1];
    assign _5 = clock;
    alinx_led_rng
        _0
        ( .clock(_5), .led4(_8[3:3]), .led3(_8[2:2]), .led2(_8[1:1]), .led1(_8[0:0]) );
    assign _12 = _8[0:0];

    /* aliases */

    /* output assignments */
    assign led1 = _12;
    assign led2 = _11;
    assign led3 = _10;
    assign led4 = _9;

endmodule
