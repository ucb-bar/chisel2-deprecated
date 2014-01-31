module StdlibSuite_PipeComp_1(input clk, input reset,
    input  io_enq_valid,
    input [7:0] io_enq_bits,
    output io_deq_valid,
    output[7:0] io_deq_bits
);

  reg[7:0] R0;
  reg[0:0] R1;
  reg[7:0] R2;
  reg[0:0] R3;

  assign io_deq_bits = R0;
  assign io_deq_valid = R3;

  always @(posedge clk) begin
    if(R1) begin
      R0 <= R2;
    end
    R1 <= reset ? 1'h0/* 0*/ : io_enq_valid;
    if(io_enq_valid) begin
      R2 <= io_enq_bits;
    end
    R3 <= reset ? 1'h0/* 0*/ : R1;
  end
endmodule

