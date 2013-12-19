module WhenSuite_SwitchClassComp_1(
    input [7:0] io_in0,
    input [7:0] io_in1,
    output [7:0] io_out
);

  wire [7:0] T0;
  wire T1;

  assign io_out = T0;
  assign T0 = T1 ? io_in1 : io_in0;
  assign T1 = io_in0 == io_in1;
endmodule

