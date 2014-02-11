module WhenSuite_WhenComp_1(
    input  io_in,
    output io_out
);

  wire T0;

  assign io_out = T0;
  assign T0 = io_in ? io_in : 1'h0;
endmodule

