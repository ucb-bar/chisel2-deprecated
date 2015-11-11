module FPAssignSuite_FpAssign_1(input clk,
    input [31:0] io_in,
    output[31:0] io_out
);

  reg [31:0] reg_;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    reg_ = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_out = reg_;

  always @(posedge clk) begin
    reg_ <= io_in;
  end
endmodule

