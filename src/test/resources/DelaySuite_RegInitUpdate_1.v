module DelaySuite_RegInitUpdate_1(input clk, input reset,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg  res;
  wire T1;
  wire T2;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    res = {1{$random}};
  end
`endif

  assign io_out = T0;
  assign T0 = {31'h0, res};
  assign T1 = reset ? 1'h0 : T2;
  assign T2 = res + 1'h1;

  always @(posedge clk) begin
    if(reset) begin
      res <= 1'h0;
    end else begin
      res <= T2;
    end
  end
endmodule

