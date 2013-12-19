module NameSuite_Block_2(input clk,
    input io_valid,
    output [31:0] io_mine_0,
    output [31:0] io_mine_1,
    //input io_sub_resp_valid
    //input io_sub_resp_bits_error
    input [31:0] io_sub_resp_bits_ppn
);

  wire [31:0] T0;
  wire T1;
  wire [31:0] T2;
  reg [31:0] tag_ram_0;
  wire [31:0] T3;
  wire [31:0] T4;
  reg [31:0] tag_ram_1;
  wire [31:0] T5;
  wire [31:0] T6;
  wire T7;

  assign io_mine_0 = T0;
  assign T0 = {31'h0/* 0*/, T1};
  assign T1 = T2[1'h0/* 0*/:1'h0/* 0*/];
  assign T2 = io_valid ? tag_ram_0 : tag_ram_1;
  assign T3 = io_valid ? T4 : tag_ram_0;
  assign T4 = io_valid ? io_sub_resp_bits_ppn : tag_ram_0;
  assign T5 = 1'h0/* 0*/ ? T4 : tag_ram_1;
  assign io_mine_1 = T6;
  assign T6 = {31'h0/* 0*/, T7};
  assign T7 = T2[1'h1/* 1*/:1'h1/* 1*/];

  always @(posedge clk) begin
    tag_ram_0 <= T3;
    tag_ram_1 <= T5;
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


  assign io_resp_resp_valid = io_imem_ptw_resp_valid;
  assign io_resp_resp_bits_error = io_imem_ptw_resp_bits_error;
  assign io_resp_resp_bits_ppn = io_imem_ptw_resp_bits_ppn;
  NameSuite_Block_2 NameSuite_Block_2(.clk(clk),
       //.io_valid(  )
       //.io_mine_0(  )
       //.io_mine_1(  )
       //.io_sub_resp_valid(  )
       //.io_sub_resp_bits_error(  )
       //.io_sub_resp_bits_ppn(  )
  );
  `ifndef SYNTHESIS
    assign NameSuite_Block_2.io_valid = $random();
    assign NameSuite_Block_2.io_sub_resp_bits_ppn = $random();
  `endif
endmodule

