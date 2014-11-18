module VerifSuite_VerilogAssertComp_1(input clk, input reset,
    input [7:0] io_x,
    input [7:0] io_y,
    output[15:0] io_z
);

  reg[0:0] T0;
  wire[15:0] T1;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    T0 = 1'b0;
  end
`endif

  assign io_z = T1;
  assign T1 = {io_x, io_y};

  always @(posedge clk) begin
`ifndef SYNTHESIS
  if(reset) T0 <= 1'b1;
  if(!reset && T0 && !reset) begin
    $fwrite(32'h80000002, "ASSERTION FAILED: %s\n", "failure");
    $finish;
  end
`endif
  end
endmodule

