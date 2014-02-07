module NameSuite_Block_2(input clk,
    input  io_in_resp_valid,
    input  io_in_resp_bits_error,
    input [31:0] io_in_resp_bits_ppn,
    output io_out_resp_valid,
    output io_out_resp_bits_error,
    output[31:0] io_out_resp_bits_ppn
);

  wire[31:0] T0;
  wire[31:0] T1;
  reg[31:0] tag_ram_1;
  wire T2;
  reg[31:0] tag_ram_0;
  wire[31:0] T3;
  wire T4;

  assign io_out_resp_bits_ppn = T0;
  assign T0 = T3 | T1;
  assign T1 = T2 ? tag_ram_1 : 32'h0;
  assign T2 = tag_ram_0[1'h1:1'h1];
  assign T3 = T4 ? tag_ram_0 : 32'h0;
  assign T4 = tag_ram_0[1'h0:1'h0];

  always @(posedge clk) begin
    if(1'h0) begin
      tag_ram_1 <= io_in_resp_bits_ppn;
    end
    if(io_in_resp_valid) begin
      tag_ram_0 <= io_in_resp_bits_ppn;
    end
  end
endmodule

module NameSuite_BindFithComp_1(input clk,
    input  io_imem_ptw_resp_valid,
    input  io_imem_ptw_resp_bits_error,
    input [31:0] io_imem_ptw_resp_bits_ppn,
    input  io_dmem_ptw_resp_valid,
    input  io_dmem_ptw_resp_bits_error,
    input [31:0] io_dmem_ptw_resp_bits_ppn,
    output io_resp_resp_valid,
    output io_resp_resp_bits_error,
    output[31:0] io_resp_resp_bits_ppn
);

  wire[31:0] vdtlb_io_out_resp_bits_ppn;
  wire vdtlb_io_out_resp_bits_error;
  wire vdtlb_io_out_resp_valid;

  assign io_resp_resp_bits_ppn = vdtlb_io_out_resp_bits_ppn;
  assign io_resp_resp_bits_error = vdtlb_io_out_resp_bits_error;
  assign io_resp_resp_valid = vdtlb_io_out_resp_valid;
  NameSuite_Block_2 vdtlb(.clk(clk),
       .io_in_resp_valid( io_imem_ptw_resp_valid ),
       .io_in_resp_bits_error( io_imem_ptw_resp_bits_error ),
       .io_in_resp_bits_ppn( io_imem_ptw_resp_bits_ppn ),
       .io_out_resp_valid( vdtlb_io_out_resp_valid ),
       .io_out_resp_bits_error( vdtlb_io_out_resp_bits_error ),
       .io_out_resp_bits_ppn( vdtlb_io_out_resp_bits_ppn )
  );
  `ifndef SYNTHESIS
    assign vdtlb.io_out_resp_valid = $random();
    assign vdtlb.io_out_resp_bits_error = $random();
  `endif
endmodule

