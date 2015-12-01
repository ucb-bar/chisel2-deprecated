module VerifSuite_VerilogAssertComp_1(input clk, input reset,
    input [7:0] io_x,
    input [7:0] io_y,
    output[15:0] io_z
);

  reg  T0;
  wire[15:0] T1;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    T0 = 1'b0;
  end
// synthesis translate_on
`endif

  assign io_z = T1;
  assign T1 = {io_x, io_y};

  always @(posedge clk) begin
`ifndef SYNTHESIS
// synthesis translate_off
  if(reset) T0 <= 1'b1;
  if(!reset && T0 && !reset) begin
    $fwrite(32'h80000002, "ASSERTION FAILED: %s\n", "failure");
    $finish;
  end
// synthesis translate_on
`endif
  end
endmodule

