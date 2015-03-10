module VerifSuite_VerilogPrintfNULComp_1(input clk, input reset,
    input  io_in
);

  wire T0;
  wire T1;
  wire[15:0] T2;


  assign T0 = reset ^ 1'h1;
  assign T1 = io_in;

  always @(posedge clk) begin
`ifndef SYNTHESIS
// synthesis translate_off
`ifdef PRINTF_COND
    if (`PRINTF_COND)
`endif
      if (T0)
        $fwrite(32'h80000002, "%b\n", T1);
// synthesis translate_on
`endif
  end
endmodule

