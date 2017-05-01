module VecSuite_UserMod_1(
    input [3:0] io_in,
    output[3:0] io_out
);

  reg [3:0] T0 = 4'h5;


  assign io_out = T0;
  always @(*) case (2'h2)
    0: T0 = 4'h3;
    1: T0 = 4'h1;
    2: T0 = 4'h5;
    3: T0 = 4'h8;
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

