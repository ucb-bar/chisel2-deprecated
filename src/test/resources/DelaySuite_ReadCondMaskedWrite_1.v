module DelaySuite_ReadCondMaskedWrite_1(input clk,
    input  io_enable,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[31:0] T1;
  wire[31:0] T2;
  wire[15:0] T3;
  wire[31:0] T4;
  wire[2:0] T5;
  wire[2:0] T6;

  assign io_out = T0;
  assign T0 = mem[T6];
  assign T2 = {16'h0/* 0*/, T3};
  assign T3 = 16'hff00/* 65280*/;
  assign T4 = T0;
  assign T5 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T6 = io_addr[2'h2/* 2*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if (io_enable && T2[0])
      mem[T5][0] <= T4[0];
    if (io_enable && T2[1])
      mem[T5][1] <= T4[1];
    if (io_enable && T2[2])
      mem[T5][2] <= T4[2];
    if (io_enable && T2[3])
      mem[T5][3] <= T4[3];
    if (io_enable && T2[4])
      mem[T5][4] <= T4[4];
    if (io_enable && T2[5])
      mem[T5][5] <= T4[5];
    if (io_enable && T2[6])
      mem[T5][6] <= T4[6];
    if (io_enable && T2[7])
      mem[T5][7] <= T4[7];
    if (io_enable && T2[8])
      mem[T5][8] <= T4[8];
    if (io_enable && T2[9])
      mem[T5][9] <= T4[9];
    if (io_enable && T2[10])
      mem[T5][10] <= T4[10];
    if (io_enable && T2[11])
      mem[T5][11] <= T4[11];
    if (io_enable && T2[12])
      mem[T5][12] <= T4[12];
    if (io_enable && T2[13])
      mem[T5][13] <= T4[13];
    if (io_enable && T2[14])
      mem[T5][14] <= T4[14];
    if (io_enable && T2[15])
      mem[T5][15] <= T4[15];
    if (io_enable && T2[16])
      mem[T5][16] <= T4[16];
    if (io_enable && T2[17])
      mem[T5][17] <= T4[17];
    if (io_enable && T2[18])
      mem[T5][18] <= T4[18];
    if (io_enable && T2[19])
      mem[T5][19] <= T4[19];
    if (io_enable && T2[20])
      mem[T5][20] <= T4[20];
    if (io_enable && T2[21])
      mem[T5][21] <= T4[21];
    if (io_enable && T2[22])
      mem[T5][22] <= T4[22];
    if (io_enable && T2[23])
      mem[T5][23] <= T4[23];
    if (io_enable && T2[24])
      mem[T5][24] <= T4[24];
    if (io_enable && T2[25])
      mem[T5][25] <= T4[25];
    if (io_enable && T2[26])
      mem[T5][26] <= T4[26];
    if (io_enable && T2[27])
      mem[T5][27] <= T4[27];
    if (io_enable && T2[28])
      mem[T5][28] <= T4[28];
    if (io_enable && T2[29])
      mem[T5][29] <= T4[29];
    if (io_enable && T2[30])
      mem[T5][30] <= T4[30];
    if (io_enable && T2[31])
      mem[T5][31] <= T4[31];
  end
endmodule

