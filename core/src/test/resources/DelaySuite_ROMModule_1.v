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
    default: begin
      T0 = 4'bx;
`ifndef SYNTHESIS
// synthesis translate_off
      T0 = {1{$random}};
// synthesis translate_on
`endif
    end
  endcase
endmodule

