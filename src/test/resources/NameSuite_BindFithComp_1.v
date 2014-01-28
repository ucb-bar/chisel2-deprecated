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
  wire[31:0] T2;
  wire[31:0] T3;
  reg[31:0] tag_ram_1;
  wire T4;
  reg[31:0] tag_ram_0;
  wire[31:0] T5;
  wire[31:0] T6;
  wire[31:0] T7;
  wire T8;

  assign io_out_resp_bits_ppn = T0;
  assign T0 = T5 | T1;
  assign T1 = T4 ? T3 : T2;
  assign T2 = {31'h0/* 0*/, 1'h0/* 0*/};
  assign T3 = tag_ram_1;
  assign T4 = tag_ram_0[1'h1/* 1*/:1'h1/* 1*/];
  assign T5 = T8 ? T7 : T6;
  assign T6 = {31'h0/* 0*/, 1'h0/* 0*/};
  assign T7 = tag_ram_0;
  assign T8 = tag_ram_0[1'h0/* 0*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if(1'h0/* 0*/) begin
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

