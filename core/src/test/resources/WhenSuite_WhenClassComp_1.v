module WhenSuite_WhenClassComp_1(
    input  io_in0,
    input  io_in1,
    output io_out
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;
  wire T4;
  wire T5;
  wire T6;

  assign io_out = T0;
  assign T0 = T5 ? 1'h0 : T1;
  assign T1 = T3 ? io_in1 : T2;
  assign T2 = io_in0 ? io_in0 : 1'h0;
  assign T3 = T4 && io_in1;
  assign T4 = ! io_in0;
  assign T5 = ! T6;
  assign T6 = io_in0 || io_in1;
endmodule

