module NameSuite_VecComp_1(input clk,
    input [63:0] io_pcr_req_data,
    input  io_r_en,
    input [4:0] io_r_addr,
    input [63:0] io_w_data,
    output[7:0] io_status_im
);

  reg [7:0] reg_status_im;
  wire[7:0] T0;
  wire[63:0] wdata;
  reg [63:0] host_pcr_bits_data;
  wire[63:0] T1;
  wire[63:0] T2;
  wire[7:0] rdata;
  wire[7:0] elts_0;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    reg_status_im = {1{$random}};
    host_pcr_bits_data = {2{$random}};
  end
// synthesis translate_on
`endif

  assign io_status_im = reg_status_im;
  assign T0 = wdata[3'h7:1'h0];
  assign wdata = io_r_en ? io_w_data : host_pcr_bits_data;
  assign T1 = io_r_en ? T2 : host_pcr_bits_data;
  assign T2 = {56'h0, rdata};
  assign rdata = elts_0;
  assign elts_0 = reg_status_im;

  always @(posedge clk) begin
    reg_status_im <= T0;
    if(io_r_en) begin
      host_pcr_bits_data <= T2;
    end
  end
endmodule

