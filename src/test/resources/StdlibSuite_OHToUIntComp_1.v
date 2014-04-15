module StdlibSuite_OHToUIntComp_1(
    input  io_in,
    output[1:0] io_out
);

  wire[1:0] T0;
  wire T1;


  assign io_out = T0;
  assign T0 = {io_in, T1};
  assign T1 = io_in || io_in;
endmodule

