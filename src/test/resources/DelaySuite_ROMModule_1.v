module DelaySuite_ROMModule_1(
    input [1:0] io_addr,
    output[3:0] io_out
);

  wire[3:0] T0;
  wire[3:0] rom_2;
  wire[3:0] rom_1;
  wire[3:0] rom_0;

  assign io_out = T0;
  assign T0 = 
    io_addr == 2'd0 ? rom_0 :
    io_addr == 2'd1 ? rom_1 :
    io_addr == 2'd2 ? rom_2 :
    $random();
  assign rom_2 = 4'h3/* 3*/;
  assign rom_1 = 4'h2/* 2*/;
  assign rom_0 = 4'h1/* 1*/;
endmodule

