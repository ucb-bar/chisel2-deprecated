module DelaySuite_ReadCondWriteModule_1(input clk,
    input  io_enable,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[31:0] T1;
  wire[31:0] T2;
  wire[31:0] T3;
  wire[2:0] T4;
  wire[31:0] T5;
  wire T6;
  wire[2:0] T7;
  wire[31:0] T8;
  wire[31:0] T9;
  wire[31:0] T10;
  wire[2:0] T11;
  wire[2:0] T12;

  assign io_out = T0;
  assign T0 = mem[T12];
  assign T2 = T3;
  assign T3 = mem[T4];
  assign T4 = T5[2'h2/* 2*/:1'h0/* 0*/];
  assign T5 = io_addr + 32'h4/* 4*/;
  assign T6 = ! io_enable;
  assign T7 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T9 = T10;
  assign T10 = T0 + 32'h1/* 1*/;
  assign T11 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T12 = io_addr[2'h2/* 2*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if (T6)
      mem[T7] <= T2;
    if (io_enable)
      mem[T11] <= T9;
  end
endmodule

