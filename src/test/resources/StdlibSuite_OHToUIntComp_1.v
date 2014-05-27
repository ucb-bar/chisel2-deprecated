module StdlibSuite_OHToUIntComp_1(
    input  io_in,
    output[1:0] io_out
);

  wire[1:0] T0;
  wire T1;
  wire[1:0] T2;
  wire[1:0] T3;
  wire[3:0] T4;
  wire[3:0] T5;
  wire[1:0] T6;
  wire[1:0] T7;
  wire[1:0] T8;
  wire T9;


  assign io_out = T0;
  assign T0 = {T9, T1};
  assign T1 = T2[1'h1:1'h1];
  assign T2 = T8 | T3;
  assign T3 = T4[1'h1:1'h0];
  assign T4 = T5;
  assign T5 = {T7, T6};
  assign T6 = {io_in, 1'h1};
  assign T7 = {io_in, 1'h0};
  assign T8 = T4[2'h3:2'h2];
  assign T9 = T8 != 2'h0;
endmodule

