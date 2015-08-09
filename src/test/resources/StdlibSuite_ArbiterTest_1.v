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
  wire[1:0] T24;
  wire[1:0] T25;
  wire[7:0] T26;
  wire[7:0] T27;
  wire T28;
  wire[1:0] T29;
  wire[7:0] T30;
  wire T31;
  wire T32;
  wire T33;
  wire T34;
  wire T35;
  wire T36;
  wire T37;
  wire T38;
  wire T39;
  wire T40;
  wire T41;
  wire T42;
  wire T43;
  wire T44;
  wire T45;
  wire T46;
  wire T47;


  assign io_chosen = chosen;
  assign chosen = choose;
  assign choose = io_in_0_valid ? 2'h0 : T24;
  assign T24 = io_in_1_valid ? 2'h1 : T25;
  assign T25 = io_in_2_valid ? 2'h2 : 2'h3;
  assign io_out_bits = T26;
  assign T26 = T32 ? T30 : T27;
  assign T27 = T28 ? io_in_1_bits : io_in_0_bits;
  assign T28 = T29[1'h0:1'h0];
  assign T29 = chosen;
  assign T30 = T31 ? io_in_3_bits : io_in_2_bits;
  assign T31 = T29[1'h0:1'h0];
  assign T32 = T29[1'h1:1'h1];
  assign io_out_valid = T33;
  assign T33 = T38 ? T36 : T34;
  assign T34 = T35 ? io_in_1_valid : io_in_0_valid;
  assign T35 = T29[1'h0:1'h0];
  assign T36 = T37 ? io_in_3_valid : io_in_2_valid;
  assign T37 = T29[1'h0:1'h0];
  assign T38 = T29[1'h1:1'h1];
  assign io_in_0_ready = io_out_ready;
  assign io_in_1_ready = T39;
  assign T39 = T40 & io_out_ready;
  assign T40 = io_in_0_valid ^ 1'h1;
  assign io_in_2_ready = T41;
  assign T41 = T42 & io_out_ready;
  assign T42 = T43 ^ 1'h1;
  assign T43 = io_in_0_valid | io_in_1_valid;
  assign io_in_3_ready = T44;
  assign T44 = T45 & io_out_ready;
  assign T45 = T46 ^ 1'h1;
  assign T46 = T47 | io_in_2_valid;
  assign T47 = io_in_0_valid | io_in_1_valid;
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

  wire T1;
  wire arb_io_in_3_ready;
  wire arb_io_in_2_ready;
  wire arb_io_in_1_ready;
  wire arb_io_in_0_ready;
  wire arb_io_out_valid;
  wire[7:0] arb_io_out_bits;
  wire[1:0] arb_io_chosen;


  assign io_fire = T1;
  assign T1 = io_out_ready & arb_io_out_valid;
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

