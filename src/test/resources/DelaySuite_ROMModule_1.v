module DelaySuite_ROMModule_1(
    input [1:0] io_addr,
    output[3:0] io_out
);

  wire[3:0] T0;
  reg [3:0] T1 [2:0];

  assign io_out = T0;
`ifndef SYNTHESIS
  assign T0 = io_addr >= 2'h3 ? {1{$random}} : T1[io_addr];
`else
  assign T0 = T1[io_addr];
`endif
  always @(*) begin
    T1[0] = 4'h1;
    T1[1] = 4'h2;
    T1[2] = 4'h3;
  end
endmodule

