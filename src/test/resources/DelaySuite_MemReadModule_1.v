module DelaySuite_MemReadModule_1(input clk,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[2:0] T1;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    for (initvar = 0; initvar < 8; initvar = initvar+1)
      mem[initvar] = {1{$random}};
  end
`endif

  assign io_out = T0;
  assign T0 = mem[T1];
  assign T1 = io_addr[2'h2:1'h0];

  always @(posedge clk) begin
  end
endmodule

