module DelaySuite_ReadCondMaskedWrite_1(input clk, input reset,
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
  wire [15:0] T6;

  assign io_out = T0;
  assign T0 = mem[T1];
  assign T1 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T3 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T4 = {29'h0/* 0*/, T7};
  assign T5 = {16'h0/* 0*/, T6};
  assign T6 =  16'hff00/* 65280*/;

  always @(posedge clk) begin
    if (io_enable && T5[0])
      mem[T3][0] <= T4[0];
    if (io_enable && T5[1])
      mem[T3][1] <= T4[1];
    if (io_enable && T5[2])
      mem[T3][2] <= T4[2];
    if (io_enable && T5[3])
      mem[T3][3] <= T4[3];
    if (io_enable && T5[4])
      mem[T3][4] <= T4[4];
    if (io_enable && T5[5])
      mem[T3][5] <= T4[5];
    if (io_enable && T5[6])
      mem[T3][6] <= T4[6];
    if (io_enable && T5[7])
      mem[T3][7] <= T4[7];
    if (io_enable && T5[8])
      mem[T3][8] <= T4[8];
    if (io_enable && T5[9])
      mem[T3][9] <= T4[9];
    if (io_enable && T5[10])
      mem[T3][10] <= T4[10];
    if (io_enable && T5[11])
      mem[T3][11] <= T4[11];
    if (io_enable && T5[12])
      mem[T3][12] <= T4[12];
    if (io_enable && T5[13])
      mem[T3][13] <= T4[13];
    if (io_enable && T5[14])
      mem[T3][14] <= T4[14];
    if (io_enable && T5[15])
      mem[T3][15] <= T4[15];
    if (io_enable && T5[16])
      mem[T3][16] <= T4[16];
    if (io_enable && T5[17])
      mem[T3][17] <= T4[17];
    if (io_enable && T5[18])
      mem[T3][18] <= T4[18];
    if (io_enable && T5[19])
      mem[T3][19] <= T4[19];
    if (io_enable && T5[20])
      mem[T3][20] <= T4[20];
    if (io_enable && T5[21])
      mem[T3][21] <= T4[21];
    if (io_enable && T5[22])
      mem[T3][22] <= T4[22];
    if (io_enable && T5[23])
      mem[T3][23] <= T4[23];
    if (io_enable && T5[24])
      mem[T3][24] <= T4[24];
    if (io_enable && T5[25])
      mem[T3][25] <= T4[25];
    if (io_enable && T5[26])
      mem[T3][26] <= T4[26];
    if (io_enable && T5[27])
      mem[T3][27] <= T4[27];
    if (io_enable && T5[28])
      mem[T3][28] <= T4[28];
    if (io_enable && T5[29])
      mem[T3][29] <= T4[29];
    if (io_enable && T5[30])
      mem[T3][30] <= T4[30];
    if (io_enable && T5[31])
      mem[T3][31] <= T4[31];
  end
endmodule

