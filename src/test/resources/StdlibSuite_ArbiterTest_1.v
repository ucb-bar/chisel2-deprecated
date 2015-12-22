module Arbiter(
    output io_in_3_ready,
    input  io_in_3_valid,
    input [7:0] io_in_3_bits,
    output io_in_2_ready,
    input  io_in_2_valid,
    input [7:0] io_in_2_bits,
    output io_in_1_ready,
    input  io_in_1_valid,
    input [7:0] io_in_1_bits,
    output io_in_0_ready,
    input  io_in_0_valid,
    input [7:0] io_in_0_bits,
    input  io_out_ready,
    output io_out_valid,
    output[7:0] io_out_bits,
    output[1:0] io_chosen
);

  wire[1:0] chosen;
  wire[1:0] choose;
  wire[1:0] T0;
  wire[1:0] T1;
  wire[7:0] T2;
  wire[7:0] T3;
  wire T4;
  wire[1:0] T5;
  wire[7:0] T6;
  wire T7;
  wire T8;
  wire T9;
  wire T10;
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


  assign io_chosen = chosen;
  assign chosen = choose;
  assign choose = io_in_0_valid ? 2'h0 : T0;
  assign T0 = io_in_1_valid ? 2'h1 : T1;
  assign T1 = io_in_2_valid ? 2'h2 : 2'h3;
  assign io_out_bits = T2;
  assign T2 = T8 ? T6 : T3;
  assign T3 = T4 ? io_in_1_bits : io_in_0_bits;
  assign T4 = T5[1'h0];
  assign T5 = chosen;
  assign T6 = T7 ? io_in_3_bits : io_in_2_bits;
  assign T7 = T5[1'h0];
  assign T8 = T5[1'h1];
  assign io_out_valid = T9;
  assign T9 = T14 ? T12 : T10;
  assign T10 = T11 ? io_in_1_valid : io_in_0_valid;
  assign T11 = T5[1'h0];
  assign T12 = T13 ? io_in_3_valid : io_in_2_valid;
  assign T13 = T5[1'h0];
  assign T14 = T5[1'h1];
  assign io_in_0_ready = io_out_ready;
  assign io_in_1_ready = T15;
  assign T15 = T16 & io_out_ready;
  assign T16 = io_in_0_valid ^ 1'h1;
  assign io_in_2_ready = T17;
  assign T17 = T18 & io_out_ready;
  assign T18 = T19 ^ 1'h1;
  assign T19 = io_in_0_valid | io_in_1_valid;
  assign io_in_3_ready = T20;
  assign T20 = T21 & io_out_ready;
  assign T21 = T22 ^ 1'h1;
  assign T22 = T23 | io_in_2_valid;
  assign T23 = io_in_0_valid | io_in_1_valid;
endmodule

module StdlibSuite_ArbiterTest_1(
    output io_in_3_ready,
    input  io_in_3_valid,
    input [7:0] io_in_3_bits,
    output io_in_2_ready,
    input  io_in_2_valid,
    input [7:0] io_in_2_bits,
    output io_in_1_ready,
    input  io_in_1_valid,
    input [7:0] io_in_1_bits,
    output io_in_0_ready,
    input  io_in_0_valid,
    input [7:0] io_in_0_bits,
    input  io_out_ready,
    output io_out_valid,
    output[7:0] io_out_bits,
    output[1:0] io_chosen,
    output io_fire
);

  wire T0;
  wire arb_io_in_3_ready;
  wire arb_io_in_2_ready;
  wire arb_io_in_1_ready;
  wire arb_io_in_0_ready;
  wire arb_io_out_valid;
  wire[7:0] arb_io_out_bits;
  wire[1:0] arb_io_chosen;


  assign io_fire = T0;
  assign T0 = io_out_ready & arb_io_out_valid;
  assign io_chosen = arb_io_chosen;
  assign io_out_bits = arb_io_out_bits;
  assign io_out_valid = arb_io_out_valid;
  assign io_in_0_ready = arb_io_in_0_ready;
  assign io_in_1_ready = arb_io_in_1_ready;
  assign io_in_2_ready = arb_io_in_2_ready;
  assign io_in_3_ready = arb_io_in_3_ready;
  Arbiter arb(
       .io_in_3_ready( arb_io_in_3_ready ),
       .io_in_3_valid( io_in_3_valid ),
       .io_in_3_bits( io_in_3_bits ),
       .io_in_2_ready( arb_io_in_2_ready ),
       .io_in_2_valid( io_in_2_valid ),
       .io_in_2_bits( io_in_2_bits ),
       .io_in_1_ready( arb_io_in_1_ready ),
       .io_in_1_valid( io_in_1_valid ),
       .io_in_1_bits( io_in_1_bits ),
       .io_in_0_ready( arb_io_in_0_ready ),
       .io_in_0_valid( io_in_0_valid ),
       .io_in_0_bits( io_in_0_bits ),
       .io_out_ready( io_out_ready ),
       .io_out_valid( arb_io_out_valid ),
       .io_out_bits( arb_io_out_bits ),
       .io_chosen( arb_io_chosen )
  );
endmodule

