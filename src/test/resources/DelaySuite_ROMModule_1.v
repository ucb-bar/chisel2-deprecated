module DelaySuite_ROMModule_1(
    input [1:0] io_addr,
    output[3:0] io_out
);

  reg [3:0] T0;


  assign io_out = T0;
  always @(*) case (io_addr)
    0: T0 = 4'h1;
    1: T0 = 4'h2;
    2: T0 = 4'h3;
`ifndef SYNTHESIS
    default: T0 = {1{$random}};
`else
    default: T0 = 4'bx;
`endif
  endcase
endmodule

