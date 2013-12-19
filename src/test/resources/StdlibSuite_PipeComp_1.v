module StdlibSuite_PipeComp_1(input clk, input reset,
    input io_enq_valid,
    input [7:0] io_enq_bits,
    output io_deq_valid,
    output [7:0] io_deq_bits
);

  wire T0;
  reg R1;
  reg R2;
  wire [7:0] T3;
  reg [7:0] R4;
  wire [7:0] T5;
  reg [7:0] R6;
  wire [7:0] T7;

  assign io_deq_valid = T0;
  assign T0 = R1;
  assign io_deq_bits = T3;
  assign T3 = R4;
  assign T5 = R2 ? R6 : R4;
  assign T7 = io_enq_valid ? io_enq_bits : R6;

  always @(posedge clk) begin
    R1 <= reset ? 1'h0/* 0*/ : R2;
    R2 <= reset ? 1'h0/* 0*/ : io_enq_valid;
    R4 <= T5;
    R6 <= T7;
  end
endmodule

