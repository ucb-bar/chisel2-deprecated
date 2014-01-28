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
  wire[31:0] T6;
  wire T7;
  wire[2:0] T8;
  wire[31:0] T9;
  wire[31:0] T10;
  wire[31:0] T11;
  wire[31:0] T12;
  wire[2:0] T13;
  wire[2:0] T14;

  assign io_out = T0;
  assign T0 = mem[T14];
  assign T2 = T3;
  assign T3 = mem[T4];
  assign T4 = T5[2'h2/* 2*/:1'h0/* 0*/];
  assign T5 = io_addr + T6;
  assign T6 = {29'h0/* 0*/, 3'h4/* 4*/};
  assign T7 = ! io_enable;
  assign T8 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T10 = T11;
  assign T11 = T0 + T12;
  assign T12 = {31'h0/* 0*/, 1'h1/* 1*/};
  assign T13 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T14 = io_addr[2'h2/* 2*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if (T7)
      mem[T8] <= T2;
    if (io_enable)
      mem[T13] <= T10;
  end
endmodule

