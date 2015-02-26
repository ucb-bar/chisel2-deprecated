module DelaySuite_RegInitUpdate_1(input clk, input reset,
    output[31:0] io_out
);

  wire[31:0] T1;
  reg  res;
  wire T2;
  wire T0;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    res = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_out = T1;
  assign T1 = {31'h0, res};
  assign T2 = reset ? 1'h0 : T0;
  assign T0 = res + 1'h1;

  always @(posedge clk) begin
    if(reset) begin
      res <= 1'h0;
    end else begin
      res <= T0;
    end
  end
endmodule

