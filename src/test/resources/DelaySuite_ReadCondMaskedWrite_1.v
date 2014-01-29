module DelaySuite_ReadCondMaskedWrite_1(input clk,
    input  io_enable,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[31:0] T1;
  wire[31:0] T2;
  wire[31:0] T3;
  wire[31:0] T4;
  wire[31:0] T5;
  wire[31:0] T6;
  wire[31:0] T7;
  wire[2:0] T8;
  wire[2:0] T9;

  assign io_out = T0;
  assign T0 = mem[T9];
  assign T2 = T3;
  assign T3 = T6 | T4;
  assign T4 = T5 & 32'hff/* 255*/;
  assign T5 = T0;
  assign T6 = T7 & 32'hff00/* 65280*/;
  assign T7 = T0;
  assign T8 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T9 = io_addr[2'h2/* 2*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if (io_enable)
      mem[T8] <= T2;
  end
endmodule

