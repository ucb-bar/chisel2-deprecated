module DelaySuite_ReadCondWriteModule_1(input clk,
    input  io_enable,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[31:0] T1;
  wire[31:0] T2;
  wire[2:0] T7;
  wire[31:0] T3;
  wire T4;
  wire[2:0] T8;
  wire[31:0] T5;
  wire[31:0] T6;
  wire[2:0] T9;
  wire[2:0] T10;

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
  assign T0 = mem[T10];
  assign T2 = mem[T7];
  assign T7 = T3[2:0];
  assign T3 = io_addr + 32'h4;
  assign T4 = io_enable ^ 1'h1;
  assign T8 = io_addr[2:0];
  assign T6 = T0 + 32'h1;
  assign T9 = io_addr[2:0];
  assign T10 = io_addr[2:0];

  always @(posedge clk) begin
    if (T4)
      mem[T8] <= T2;
    if (io_enable)
      mem[T9] <= T6;
  end
endmodule

