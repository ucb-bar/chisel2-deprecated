module VerifSuite_VerilogPrintfComp_1(input clk, input reset,
    input [7:0] io_x,
    input [7:0] io_y,
    output[15:0] io_z
);

  wire T0;
  wire[7:0] T1;
  wire[7:0] T2;
  wire[31:0] T3;
  wire[31:0] T4;
  reg [31:0] tsc_reg;
  wire[31:0] T8;
  wire[31:0] T5;
  wire[199:0] T6;
  wire[15:0] T7;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    tsc_reg = {1{$random}};
  end
// synthesis translate_on
`endif

  assign T0 = reset ^ 1'h1;
  assign T1 = io_y;
  assign T2 = io_x;
  assign T3 = T4;
  assign T4 = tsc_reg;
  assign T8 = reset ? 32'h0 : T5;
  assign T5 = tsc_reg + 32'h1;
  assign io_z = T7;
  assign T7 = {io_x, io_y};

  always @(posedge clk) begin
    if(reset) begin
      tsc_reg <= 32'h0;
    end else begin
      tsc_reg <= T5;
    end
`ifndef SYNTHESIS
// synthesis translate_off
`ifdef PRINTF_COND
    if (`PRINTF_COND)
`endif
      if (T0)
        $fwrite(32'h80000002, "Cyc= %d io: %h %h", T3, T2, T1);
// synthesis translate_on
`endif
  end
endmodule

