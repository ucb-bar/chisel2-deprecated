module StdlibSuite_OHToUIntComp_1(
    input  io_in,
    output[1:0] io_out
);

  wire[1:0] T5;
  wire T6;
  wire[1:0] T7;
  wire[1:0] T8;
  wire[3:0] T1;
  wire[3:0] T2;
  wire[1:0] T3;
  wire[1:0] T4;
  wire[1:0] T9;
  wire T10;


  assign io_out = T5;
  assign T5 = {T10, T6};
  assign T6 = T7[1];
  assign T7 = T9 | T8;
  assign T8 = T1[1:0];
  assign T1 = T2;
  assign T2 = {T4, T3};
  assign T3 = {io_in, 1'h1};
  assign T4 = {io_in, 1'h0};
  assign T9 = T1[3:2];
  assign T10 = T9 != 2'h0;
endmodule

