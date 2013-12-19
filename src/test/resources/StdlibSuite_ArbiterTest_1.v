module StdlibSuite_ArbiterTest_1(
    output io_in_0_ready,
    input io_in_0_valid,
    input [7:0] io_in_0_bits,
    output io_in_1_ready,
    input io_in_1_valid,
    input [7:0] io_in_1_bits,
    output io_in_2_ready,
    input io_in_2_valid,
    input [7:0] io_in_2_bits,
    output io_in_3_ready,
    input io_in_3_valid,
    input [7:0] io_in_3_bits,
    input io_out_ready,
    output io_out_valid,
    output [7:0] io_out_bits,
    output [1:0] io_chosen
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;
  wire T4;
  wire T5;
  wire T6;
  wire T7;
  wire T8;
  wire T9;
  wire T10;
  wire T11;
  wire T12;
  wire T13;
  wire T14;
  wire T15;
  wire [1:0] T16;
  wire [1:0] T17;
  wire [1:0] T18;
  wire [1:0] T19;
  wire T20;
  wire T21;
  wire T22;
  wire T23;
  wire [7:0] T24;
  wire T25;
  wire [7:0] T26;
  wire T27;
  wire [7:0] T28;
  wire T29;

  assign io_in_0_ready = T0;
  assign T0 = T1 && io_out_ready;
  assign T1 = 1'h1/* 1*/;
  assign io_in_1_ready = T2;
  assign T2 = T3 && io_out_ready;
  assign T3 = T4;
  assign T4 = ! io_in_0_valid;
  assign io_in_2_ready = T5;
  assign T5 = T6 && io_out_ready;
  assign T6 = T7;
  assign T7 = ! T8;
  assign T8 = io_in_0_valid || io_in_1_valid;
  assign io_in_3_ready = T9;
  assign T9 = T10 && io_out_ready;
  assign T10 = T11;
  assign T11 = ! T12;
  assign T12 = T13 || io_in_2_valid;
  assign T13 = io_in_0_valid || io_in_1_valid;
  assign io_out_valid = T14;
  assign T14 = T15 ? T20 : T22;
  assign T15 = T16[1'h1/* 1*/:1'h1/* 1*/];
  assign T16 = T17;
  assign T17 = io_in_0_valid ? 1'h0/* 0*/ : T18;
  assign T18 = io_in_1_valid ? 1'h1/* 1*/ : T19;
  assign T19 = io_in_2_valid ? 2'h2/* 2*/ : 2'h3/* 3*/;
  assign T20 = T21 ? io_in_3_valid : io_in_2_valid;
  assign T21 = T16[1'h0/* 0*/:1'h0/* 0*/];
  assign T22 = T23 ? io_in_1_valid : io_in_0_valid;
  assign T23 = T16[1'h0/* 0*/:1'h0/* 0*/];
  assign io_out_bits = T24;
  assign T24 = T25 ? T26 : T28;
  assign T25 = T16[1'h1/* 1*/:1'h1/* 1*/];
  assign T26 = T27 ? io_in_3_bits : io_in_2_bits;
  assign T27 = T16[1'h0/* 0*/:1'h0/* 0*/];
  assign T28 = T29 ? io_in_1_bits : io_in_0_bits;
  assign T29 = T16[1'h0/* 0*/:1'h0/* 0*/];
  assign io_chosen = T16;
endmodule

