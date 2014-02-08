module NameSuite_VecComp_1(input clk,
    input  io_r_en,
    input [4:0] io_r_addr,
    input [63:0] io_w_data,
    output[7:0] io_status_im
);

  reg[7:0] reg_status_im;
  wire[7:0] T0;
  wire[63:0] wdata;
  reg[63:0] host_pcr_bits_data;
  wire[63:0] T1;
  wire[7:0] rdata;
  wire[7:0] elts_0;
  wire[7:0] T2;

  assign io_status_im = reg_status_im;
  assign T0 = wdata[3'h7:1'h0];
  assign wdata = io_r_en ? io_w_data : host_pcr_bits_data;
  assign T1 = {56'h0, rdata};
  assign rdata = elts_0;
  assign elts_0 = T2;
  assign T2 = {reg_status_im};

  always @(posedge clk) begin
    reg_status_im <= T0;
    if(io_r_en) begin
      host_pcr_bits_data <= T1;
    end
  end
endmodule

