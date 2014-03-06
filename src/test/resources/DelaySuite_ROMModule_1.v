module DelaySuite_ROMModule_1(
    input [1:0] io_addr,
    output[3:0] io_out
);

  wire[3:0] T0;
  wire[3:0] rom_2c;
  wire[3:0] rom_1b;
  wire[3:0] rom_0a;

  assign io_out = T0;
  assign T0 = 
      io_addr == 2'd0 ? 4'h1
    : io_addr == 2'd1 ? 4'h2
    : io_addr == 2'd2 ? 4'h3
`ifndef SYNTHESIS
    :$random()
`endif
    ;
  assign rom_2c = 4'h3;
  assign rom_1b = 4'h2;
  assign rom_0a = 4'h1;
endmodule

