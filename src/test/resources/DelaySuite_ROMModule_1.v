module DelaySuite_ROMModule_1(
    input [1:0] io_addr,
    output[3:0] io_out
);

  wire[3:0] T0;
  wire[3:0] c;
  wire[3:0] b;
  wire[3:0] a;

  assign io_out = T0;
  assign T0 = 
      io_addr == 2'd0 ? 4'h1
    : io_addr == 2'd1 ? 4'h2
    : io_addr == 2'd2 ? 4'h3
`ifndef SYNTHESIS
    :$random()
`endif
    ;
  assign c = 4'h3;
  assign b = 4'h2;
  assign a = 4'h1;
endmodule

