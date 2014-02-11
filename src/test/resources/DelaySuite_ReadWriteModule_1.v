module DelaySuite_ReadWriteModule_1(input clk,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[31:0] T1;
  wire[31:0] T2;
  wire[31:0] T3;
  wire[2:0] T4;
  wire[2:0] T5;

  assign io_out = T0;
  assign T0 = mem[T5];
  assign T2 = T3;
  assign T3 = T0 + 32'h1;
  assign T4 = io_addr[2'h2:1'h0];
  assign T5 = io_addr[2'h2:1'h0];

  always @(posedge clk) begin
    if (1'h1)
      mem[T4] <= T2;
  end
endmodule

