module DelaySuite_MemReadModule_1(input clk,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[2:0] T1;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    for (initvar = 0; initvar < 8; initvar = initvar+1)
      mem[initvar] = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_out = T0;
  assign T0 = mem[T1];
  assign T1 = io_addr[2:0];
endmodule

