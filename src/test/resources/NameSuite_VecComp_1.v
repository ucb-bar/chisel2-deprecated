module NameSuite_VecComp_1(input clk,
    input io_r_en,
    input [4:0] io_r_addr,
    input [63:0] io_w_data,
    output [7:0] io_status_im
);

  reg [7:0] reg_status_im;
  wire [7:0] T0;
  wire [7:0] T1;
  wire [63:0] wdata;
  reg [63:0] host_pcr_bits_data;
  wire [7:0] T2;
  wire [7:0] rdata;

  assign io_status_im = reg_status_im;
  assign T0 = T1;
  assign T1 = wdata[3'h7/* 7*/:1'h0/* 0*/];
  assign wdata = io_r_en ? io_w_data : host_pcr_bits_data;
  assign T2 = io_r_en ? rdata : host_pcr_bits_data;
  assign rdata = reg_status_im;

  always @(posedge clk) begin
    reg_status_im <= T0;
    host_pcr_bits_data <= T2;
  end
endmodule

