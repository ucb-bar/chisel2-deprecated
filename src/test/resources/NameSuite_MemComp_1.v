module NameSuite_MemComp_1(input clk, input reset,
    input io_ren,
    input [7:0] io_raddr,
    output [64:0] io_rdata
);

  wire [64:0] T0;
  reg [7:0] raddr;
  wire [7:0] T1;

  assign io_rdata = T0;
  NameSuite_MemComp_1_rfile rfile (
    .CLK(clk),
    .RST(reset),
    .R0A(raddr),
    .R0E(1'h1/* 1*/),
    .R0O(T0)
  );
  assign T1 = io_ren ? io_raddr : raddr;

  always @(posedge clk) begin
    raddr <= T1;
  end
endmodule

