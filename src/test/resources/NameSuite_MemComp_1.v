module NameSuite_MemComp_1(input clk,
    input  io_ren,
    input [7:0] io_raddr,
    output[64:0] io_rdata
);

  wire[64:0] T0;
  reg [7:0] raddr;
  wire[7:0] T1;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    raddr = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_rdata = T0;
  NameSuite_MemComp_1_rfile rfile (
    .CLK(clk),
    .R0A(io_raddr),
    .R0E(io_ren),
    .R0O(T0)
  );
  assign T1 = io_ren ? io_raddr : raddr;

  always @(posedge clk) begin
    if(io_ren) begin
      raddr <= io_raddr;
    end
  end
endmodule

