module DelaySuite_RegInitCondUpdate_1(input clk, input reset,
    input  io_in,
    output[31:0] io_out
);

  wire[31:0] T2;
  reg  res;
  wire T3;
  wire T0;
  wire T1;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    res = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_out = T2;
  assign T2 = {31'h0, res};
  assign T3 = reset ? 1'h0 : T0;
  assign T0 = io_in ? T1 : res;
  assign T1 = res + 1'h1;

  always @(posedge clk) begin
    if(reset) begin
      res <= 1'h0;
    end else if(io_in) begin
      res <= T1;
    end
  end
endmodule

