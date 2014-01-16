module NameSuite_Block_2(input clk,
    input io_in_resp_valid,
    input io_in_resp_bits_error,
    input [31:0] io_in_resp_bits_ppn,
    output io_out_resp_valid,
    output io_out_resp_bits_error,
    output [31:0] io_out_resp_bits_ppn
);

  wire [31:0] tag_ram_0;
  wire [31:0] T0;
  wire T1;
  reg [31:0] R2;
  wire [31:0] T3;
  wire [31:0] T4;
  wire [31:0] T5;
  wire T6;
  reg [31:0] tag_ram_1;
  wire [31:0] T7;

  assign io_out_resp_bits_ppn = tag_ram_0;
  assign tag_ram_0 = T0 | T5;
  assign T0 = T1 ? R2 : 1'h0/* 0*/;
  assign T1 = R2[1'h0/* 0*/:1'h0/* 0*/];
  assign T3 = io_in_resp_valid ? T4 : R2;
  assign T4 = io_in_resp_valid ? io_in_resp_bits_ppn : R2;
  assign T5 = T6 ? tag_ram_1 : 1'h0/* 0*/;
  assign T6 = R2[1'h1/* 1*/:1'h1/* 1*/];
  assign T7 = 1'h0/* 0*/ ? T4 : tag_ram_1;

  always @(posedge clk) begin
    R2 <= T3;
    tag_ram_1 <= T7;
  end
endmodule

module NameSuite_BindFithComp_1(input clk,
    input io_imem_ptw_resp_valid,
    input io_imem_ptw_resp_bits_error,
    input [31:0] io_imem_ptw_resp_bits_ppn,
    input io_dmem_ptw_resp_valid,
    input io_dmem_ptw_resp_bits_error,
    input [31:0] io_dmem_ptw_resp_bits_ppn,
    output io_resp_resp_valid,
    output io_resp_resp_bits_error,
    output [31:0] io_resp_resp_bits_ppn
);

  wire vdtlb_io_out_resp_valid;
  wire vdtlb_io_out_resp_bits_error;
  wire [31:0] vdtlb_io_out_resp_bits_ppn;

  assign io_resp_resp_valid = vdtlb_io_out_resp_valid;
  assign io_resp_resp_bits_error = vdtlb_io_out_resp_bits_error;
  assign io_resp_resp_bits_ppn = vdtlb_io_out_resp_bits_ppn;
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

