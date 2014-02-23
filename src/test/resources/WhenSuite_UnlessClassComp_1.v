module WhenSuite_UnlessClassComp_1(
    input  io_in0,
    input  io_in1,
    output io_out
);

  wire T0;
  wire T1;

  assign io_out = T0;
  assign T0 = T1 ? io_in1 : io_in0;
  assign T1 = ! io_in0;
endmodule

