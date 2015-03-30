module ConnectSuite_A_4(input clk,
    output[7:0] io_status_im,
    output[6:0] io_status_zero,
    output io_status_vm,
    output io_status_s64,
    output io_status_u64,
    output io_status_s,
    output io_status_ps,
    output io_status_ec,
    output io_status_ev,
    output io_status_ef,
    output io_status_et,
    input  io_wen,
    input [31:0] io_wdata
);

  reg[0:0] reg_status_et;
  wire T0;
  reg[0:0] reg_status_ef;
  wire T1;
  reg[0:0] reg_status_ev;
  wire T2;
  reg[0:0] reg_status_ec;
  wire T3;
  reg[0:0] reg_status_ps;
  wire T4;
  reg[0:0] reg_status_s;
  wire T5;
  reg[0:0] reg_status_u64;
  wire T6;
  reg[0:0] reg_status_s64;
  wire T7;
  reg[0:0] reg_status_vm;
  wire T8;
  reg[6:0] reg_status_zero;
  wire[6:0] T9;
  reg[7:0] reg_status_im;
  wire[7:0] T10;

  assign io_status_et = reg_status_et;
  assign T0 = io_wdata[1'h0:1'h0];
  assign io_status_ef = reg_status_ef;
  assign T1 = io_wdata[1'h1:1'h1];
  assign io_status_ev = reg_status_ev;
  assign T2 = io_wdata[2'h2:2'h2];
  assign io_status_ec = reg_status_ec;
  assign T3 = io_wdata[2'h3:2'h3];
  assign io_status_ps = reg_status_ps;
  assign T4 = io_wdata[3'h4:3'h4];
  assign io_status_s = reg_status_s;
  assign T5 = io_wdata[3'h5:3'h5];
  assign io_status_u64 = reg_status_u64;
  assign T6 = io_wdata[3'h6:3'h6];
  assign io_status_s64 = reg_status_s64;
  assign T7 = io_wdata[3'h7:3'h7];
  assign io_status_vm = reg_status_vm;
  assign T8 = io_wdata[4'h8:4'h8];
  assign io_status_zero = reg_status_zero;
  assign T9 = io_wdata[4'hf:4'h9];
  assign io_status_im = reg_status_im;
  assign T10 = io_wdata[5'h17:5'h10];

  always @(posedge clk) begin
    if(io_wen) begin
      reg_status_et <= T0;
    end
    if(io_wen) begin
      reg_status_ef <= T1;
    end
    if(io_wen) begin
      reg_status_ev <= T2;
    end
    if(io_wen) begin
      reg_status_ec <= T3;
    end
    if(io_wen) begin
      reg_status_ps <= T4;
    end
    if(io_wen) begin
      reg_status_s <= T5;
    end
    if(io_wen) begin
      reg_status_u64 <= T6;
    end
    if(io_wen) begin
      reg_status_s64 <= T7;
    end
    if(io_wen) begin
      reg_status_vm <= T8;
    end
    if(io_wen) begin
      reg_status_zero <= T9;
    end
    if(io_wen) begin
      reg_status_im <= T10;
    end
  end
endmodule

