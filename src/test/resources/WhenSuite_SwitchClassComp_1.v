module WhenSuite_SwitchClassComp_1(
    input [7:0] io_in0,
    input [7:0] io_in1,
    output[7:0] io_out
);

  wire[7:0] T0;
  wire T1;
  wire T2;
  wire[7:0] T3;
  wire[7:0] T4;

  assign io_out = T0;
  assign T0 = T1 ? io_in1 : io_in0;
  assign T1 = T2;
  assign T2 = T4 == T3;
  assign T3 = {1'h0, 7'h51};
  assign T4 = io_in0 & 8'hf3;
endmodule

