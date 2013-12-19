module WhenSuite_WhenClassComp_1(
    input io_in0,
    input io_in1,
    output io_out
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;

  assign io_out = T0;
  assign T0 = T1 ? io_in1 : T3;
  assign T1 = T2 && io_in1;
  assign T2 = ! io_in0;
  assign T3 = io_in0 ? io_in0 : 1'h0/* 0*/;
endmodule

