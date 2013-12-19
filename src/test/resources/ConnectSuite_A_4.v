module ConnectSuite_A_4(input clk,
    output [7:0] io_status_im,
    output [6:0] io_status_zero,
    output io_status_vm,
    output io_status_s64,
    output io_status_u64,
    output io_status_s,
    output io_status_ps,
    output io_status_ec,
    output io_status_ev,
    output io_status_ef,
    output io_status_et,
    input io_wen,
    input [31:0] io_wdata
);

  reg [7:0] reg_status_im;
  wire [7:0] T0;
  wire [7:0] T1;
  wire [7:0] T2;
  wire [7:0] T3;
  reg [6:0] reg_status_zero;
  wire [6:0] T4;
  wire [6:0] T5;
  wire [6:0] T6;
  wire [6:0] T7;
  reg reg_status_vm;
  wire T8;
  wire T9;
  wire T10;
  wire T11;
  reg reg_status_s64;
  wire T12;
  wire T13;
  wire T14;
  wire T15;
  reg reg_status_u64;
  wire T16;
  wire T17;
  wire T18;
  wire T19;
  reg reg_status_s;
  wire T20;
  wire T21;
  wire T22;
  wire T23;
  reg reg_status_ps;
  wire T24;
  wire T25;
  wire T26;
  wire T27;
  reg reg_status_ec;
  wire T28;
  wire T29;
  wire T30;
  wire T31;
  reg reg_status_ev;
  wire T32;
  wire T33;
  wire T34;
  wire T35;
  reg reg_status_ef;
  wire T36;
  wire T37;
  wire T38;
  wire T39;
  reg reg_status_et;
  wire T40;
  wire T41;
  wire T42;
  wire T43;

  assign io_status_im = reg_status_im;
  assign T0 = io_wen ? T1 : reg_status_im;
  assign T1 = T2;
  assign T2 = T3;
  assign T3 = io_wdata[5'h17/* 23*/:5'h10/* 16*/];
  assign io_status_zero = reg_status_zero;
  assign T4 = io_wen ? T5 : reg_status_zero;
  assign T5 = T6;
  assign T6 = T7;
  assign T7 = io_wdata[4'hf/* 15*/:4'h9/* 9*/];
  assign io_status_vm = reg_status_vm;
  assign T8 = io_wen ? T9 : reg_status_vm;
  assign T9 = T10;
  assign T10 = T11;
  assign T11 = io_wdata[4'h8/* 8*/:4'h8/* 8*/];
  assign io_status_s64 = reg_status_s64;
  assign T12 = io_wen ? T13 : reg_status_s64;
  assign T13 = T14;
  assign T14 = T15;
  assign T15 = io_wdata[3'h7/* 7*/:3'h7/* 7*/];
  assign io_status_u64 = reg_status_u64;
  assign T16 = io_wen ? T17 : reg_status_u64;
  assign T17 = T18;
  assign T18 = T19;
  assign T19 = io_wdata[3'h6/* 6*/:3'h6/* 6*/];
  assign io_status_s = reg_status_s;
  assign T20 = io_wen ? T21 : reg_status_s;
  assign T21 = T22;
  assign T22 = T23;
  assign T23 = io_wdata[3'h5/* 5*/:3'h5/* 5*/];
  assign io_status_ps = reg_status_ps;
  assign T24 = io_wen ? T25 : reg_status_ps;
  assign T25 = T26;
  assign T26 = T27;
  assign T27 = io_wdata[3'h4/* 4*/:3'h4/* 4*/];
  assign io_status_ec = reg_status_ec;
  assign T28 = io_wen ? T29 : reg_status_ec;
  assign T29 = T30;
  assign T30 = T31;
  assign T31 = io_wdata[2'h3/* 3*/:2'h3/* 3*/];
  assign io_status_ev = reg_status_ev;
  assign T32 = io_wen ? T33 : reg_status_ev;
  assign T33 = T34;
  assign T34 = T35;
  assign T35 = io_wdata[2'h2/* 2*/:2'h2/* 2*/];
  assign io_status_ef = reg_status_ef;
  assign T36 = io_wen ? T37 : reg_status_ef;
  assign T37 = T38;
  assign T38 = T39;
  assign T39 = io_wdata[1'h1/* 1*/:1'h1/* 1*/];
  assign io_status_et = reg_status_et;
  assign T40 = io_wen ? T41 : reg_status_et;
  assign T41 = T42;
  assign T42 = T43;
  assign T43 = io_wdata[1'h0/* 0*/:1'h0/* 0*/];

  always @(posedge clk) begin
    reg_status_im <= T0;
    reg_status_zero <= T4;
    reg_status_vm <= T8;
    reg_status_s64 <= T12;
    reg_status_u64 <= T16;
    reg_status_s <= T20;
    reg_status_ps <= T24;
    reg_status_ec <= T28;
    reg_status_ev <= T32;
    reg_status_ef <= T36;
    reg_status_et <= T40;
  end
endmodule

