module DelaySuite_ReadCondWriteModule_1(input clk, input reset,
    input io_enable,
    input [31:0] io_addr,
    output [31:0] io_out
);

  wire [31:0] T0;
  reg [31:0] mem [7:0];
  wire [2:0] T1;
  wire [31:0] T2;
  wire [2:0] T3;
  wire [31:0] T4;
  wire [31:0] T5;
  wire [31:0] T6;
  wire [2:0] T7;
  wire [31:0] T8;
  wire [2:0] T9;
  wire [31:0] T10;
  wire [2:0] T11;
  wire [31:0] T12;
  wire T13;

  assign io_out = T0;
  assign T0 = mem[T1];
  assign T1 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T3 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T4 = T5;
  assign T5 = T6 + 32'h1/* 1*/;
  assign T6 = mem[T7];
  assign T7 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T9 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T10 = mem[T11];
  assign T11 = T12[2'h2/* 2*/:1'h0/* 0*/];
  assign T12 = io_addr + 32'h4/* 4*/;
  assign T13 = ! io_enable;

  always @(posedge clk) begin
    if (io_enable)
      mem[T3] <= T4;
    if (T13)
      mem[T9] <= T10;
  end
endmodule

