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
  reg [31:0] tag_ram_1;
  wire[31:0] T2;
  wire T3;
  wire T4;
  wire[1:0] T5;
  wire T6;
  wire T7;
  reg [31:0] tag_ram_0;
  wire[31:0] T8;
  wire T9;
  wire T10;
  wire[31:0] T11;
  wire T12;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    tag_ram_1 = {1{$random}};
    tag_ram_0 = {1{$random}};
  end
// synthesis translate_on
`endif

`ifndef SYNTHESIS
// synthesis translate_off
  assign io_out_resp_bits_error = {1{$random}};
  assign io_out_resp_valid = {1{$random}};
// synthesis translate_on
`endif
  assign io_out_resp_bits_ppn = T0;
  assign T0 = T11 | T1;
  assign T1 = T7 ? tag_ram_1 : 32'h0;
  assign T2 = T3 ? io_in_resp_bits_ppn : tag_ram_1;
  assign T3 = io_in_resp_valid & T4;
  assign T4 = T5[1'h1];
  assign T5 = 1'h1 << T6;
  assign T6 = 1'h0;
  assign T7 = tag_ram_0[1'h1];
  assign T8 = T9 ? io_in_resp_bits_ppn : tag_ram_0;
  assign T9 = io_in_resp_valid & T10;
  assign T10 = T5[1'h0];
  assign T11 = T12 ? tag_ram_0 : 32'h0;
  assign T12 = tag_ram_0[1'h0];

  always @(posedge clk) begin
    if(T3) begin
      tag_ram_1 <= io_in_resp_bits_ppn;
    end
    if(T9) begin
      tag_ram_0 <= io_in_resp_bits_ppn;
    end
  end
endmodule

module NameSuite_BindFifthComp_1(input clk,
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

  wire vdtlb_io_out_resp_valid;
  wire vdtlb_io_out_resp_bits_error;
  wire[31:0] vdtlb_io_out_resp_bits_ppn;


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
// synthesis translate_off
    assign vdtlb.io_out_resp_valid = {1{$random}};
    assign vdtlb.io_out_resp_bits_error = {1{$random}};
// synthesis translate_on
`endif
endmodule

