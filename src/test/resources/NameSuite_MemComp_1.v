module NameSuite_MemComp_1(input clk, input reset,
    input  io_ren,
    input [7:0] io_raddr,
    output[64:0] io_rdata
);

  wire[64:0] T0;
  reg[7:0] raddr;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.001;
`ifdef RANDOM_SEED
    initvar = $random(`RANDOM_SEED);
`endif
    #0.001;
    raddr = {1{$random}};
  end
`endif

  assign io_rdata = T0;
  NameSuite_MemComp_1_rfile rfile (
    .CLK(clk),
    .RST(reset),
    .R0A(io_raddr),
    .R0E(io_ren),
    .R0O(T0)
  );

  always @(posedge clk) begin
    if(io_ren) begin
      raddr <= io_raddr;
    end
  end
endmodule

