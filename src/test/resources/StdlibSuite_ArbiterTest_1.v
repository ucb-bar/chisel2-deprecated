module StdlibSuite_ArbiterTest_1(
    output io_in_0_ready,
    input  io_in_0_valid,
    input [7:0] io_in_0_bits,
    output io_in_1_ready,
    input  io_in_1_valid,
    input [7:0] io_in_1_bits,
    output io_in_2_ready,
    input  io_in_2_valid,
    input [7:0] io_in_2_bits,
    output io_in_3_ready,
    input  io_in_3_valid,
    input [7:0] io_in_3_bits,
    input  io_out_ready,
    output io_out_valid,
    output[7:0] io_out_bits,
    output[1:0] io_chosen
);

  wire[1:0] T0;
  wire[1:0] T1;
  wire[1:0] T2;
  wire[1:0] T3;
  wire[1:0] T4;
  wire[1:0] T5;
  wire[7:0] T6;
  wire[7:0] T7;
  wire T8;
  wire[1:0] T9;
  wire[7:0] T10;
  wire T11;
  wire T12;
  wire T13;
  wire T14;
  wire T15;
  wire T16;
  wire T17;
  wire T18;
  wire T19;
  wire T20;
  wire T21;
  wire T22;
  wire T23;
  wire T24;
  wire T25;
  wire T26;
  wire T27;
  wire T28;
  wire T29;
  wire T30;
  wire T31;
  wire T32;

  assign io_chosen = T0;
  assign T0 = T1;
  assign T1 = io_in_0_valid ? T5 : T2;
  assign T2 = io_in_1_valid ? T4 : T3;
  assign T3 = io_in_2_valid ? 2'h2/* 2*/ : 2'h3/* 3*/;
  assign T4 = {1'h0/* 0*/, 1'h1/* 1*/};
  assign T5 = {1'h0/* 0*/, 1'h0/* 0*/};
  assign io_out_bits = T6;
  assign T6 = T12 ? T10 : T7;
  assign T7 = T8 ? io_in_1_bits : io_in_0_bits;
  assign T8 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T9 = T0;
  assign T10 = T11 ? io_in_3_bits : io_in_2_bits;
  assign T11 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T12 = T9[1'h1/* 1*/:1'h1/* 1*/];
  assign io_out_valid = T13;
  assign T13 = T18 ? T16 : T14;
  assign T14 = T15 ? io_in_1_valid : io_in_0_valid;
  assign T15 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T16 = T17 ? io_in_3_valid : io_in_2_valid;
  assign T17 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T18 = T9[1'h1/* 1*/:1'h1/* 1*/];
  assign io_in_3_ready = T19;
  assign T19 = T20 && io_out_ready;
  assign T20 = T21;
  assign T21 = ! T22;
  assign T22 = T23 || io_in_2_valid;
  assign T23 = io_in_0_valid || io_in_1_valid;
  assign io_in_2_ready = T24;
  assign T24 = T25 && io_out_ready;
  assign T25 = T26;
  assign T26 = ! T27;
  assign T27 = io_in_0_valid || io_in_1_valid;
  assign io_in_1_ready = T28;
  assign T28 = T29 && io_out_ready;
  assign T29 = T30;
  assign T30 = ! io_in_0_valid;
  assign io_in_0_ready = T31;
  assign T31 = T32 && io_out_ready;
  assign T32 = 1'h1/* 1*/;
endmodule

