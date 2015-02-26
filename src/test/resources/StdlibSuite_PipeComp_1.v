module StdlibSuite_PipeComp_1(input clk, input reset,
    input  io_enq_valid,
    input [7:0] io_enq_bits,
    output io_deq_valid,
    output[7:0] io_deq_bits
);

  reg [7:0] R0;
  wire[7:0] T1;
  reg [7:0] R2;
  wire[7:0] T3;
  reg  R4;
  wire T6;
  reg  R5;
  wire T7;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    R0 = {1{$random}};
    R2 = {1{$random}};
    R4 = {1{$random}};
    R5 = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_deq_bits = R0;
  assign T1 = R4 ? R2 : R0;
  assign T3 = io_enq_valid ? io_enq_bits : R2;
  assign T6 = reset ? 1'h0 : io_enq_valid;
  assign io_deq_valid = R5;
  assign T7 = reset ? 1'h0 : R4;

  always @(posedge clk) begin
    if(R4) begin
      R0 <= R2;
    end
    if(io_enq_valid) begin
      R2 <= io_enq_bits;
    end
    if(reset) begin
      R4 <= 1'h0;
    end else begin
      R4 <= io_enq_valid;
    end
    if(reset) begin
      R5 <= 1'h0;
    end else begin
      R5 <= R4;
    end
  end
endmodule

