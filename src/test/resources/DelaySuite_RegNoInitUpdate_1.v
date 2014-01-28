module DelaySuite_RegNoInitUpdate_1(input clk,
    output[31:0] io_out
);

  reg[31:0] res;
  wire[31:0] T0;
  wire[31:0] T1;

  assign io_out = res;
  assign T0 = res + T1;
  assign T1 = {31'h0/* 0*/, 1'h1/* 1*/};

  always @(posedge clk) begin
    res <= T0;
  end
endmodule

