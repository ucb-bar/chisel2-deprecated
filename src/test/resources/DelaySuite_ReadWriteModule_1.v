module DelaySuite_ReadWriteModule_1(input clk,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[31:0] T1;
  wire[31:0] T2;
  wire[2:0] T3;
  wire[2:0] T4;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    for (initvar = 0; initvar < 8; initvar = initvar+1)
      mem[initvar] = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_out = T0;
  assign T0 = mem[T4];
  assign T2 = T0 + 32'h1;
  assign T3 = io_addr[2'h2:1'h0];
  assign T4 = io_addr[2'h2:1'h0];

  always @(posedge clk) begin
    if (1'h1)
      mem[T3] <= T2;
  end
endmodule

