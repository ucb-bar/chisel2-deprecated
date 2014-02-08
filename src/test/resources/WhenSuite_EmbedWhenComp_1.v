module WhenSuite_EmbedWhenComp_1(
    input  io_in0,
    input  io_in1,
    output io_out
);

  wire T0;
  wire T1;
  wire T2;

  assign io_out = T0;
  assign T0 = T2 ? io_in1 : T1;
  assign T1 = io_in0 ? io_in0 : 1'h0;
  assign T2 = io_in0 && io_in1;
endmodule

