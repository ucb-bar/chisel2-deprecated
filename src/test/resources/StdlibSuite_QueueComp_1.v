module Queue(input clk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input [7:0] io_enq_bits,
    input  io_deq_ready,
    output io_deq_valid,
    output[7:0] io_deq_bits
);

  wire[7:0] T0;
  reg [7:0] ram [1:0];
  wire[7:0] T1;
  wire do_enq;
  reg  R2;
  wire T3;
  wire T4;
  wire T5;
  reg  R6;
  wire T7;
  wire T8;
  wire T9;
  wire do_deq;
  wire T10;
  wire empty;
  wire T11;
  reg  maybe_full;
  wire T12;
  wire T13;
  wire T14;
  wire ptr_match;
  wire T15;
  wire full;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    for (initvar = 0; initvar < 2; initvar = initvar+1)
      ram[initvar] = {1{$random}};
    R2 = {1{$random}};
    R6 = {1{$random}};
    maybe_full = {1{$random}};
  end
`endif

  assign io_deq_bits = T0;
  assign T0 = ram[R6];
  assign do_enq = io_enq_ready & io_enq_valid;
  assign T3 = reset ? 1'h0 : T4;
  assign T4 = do_enq ? T5 : R2;
  assign T5 = R2 + 1'h1;
  assign T7 = reset ? 1'h0 : T8;
  assign T8 = do_deq ? T9 : R6;
  assign T9 = R6 + 1'h1;
  assign do_deq = io_deq_ready & io_deq_valid;
  assign io_deq_valid = T10;
  assign T10 = empty ^ 1'h1;
  assign empty = ptr_match & T11;
  assign T11 = maybe_full ^ 1'h1;
  assign T12 = reset ? 1'h0 : T13;
  assign T13 = T14 ? do_enq : maybe_full;
  assign T14 = do_enq != do_deq;
  assign ptr_match = R2 == R6;
  assign io_enq_ready = T15;
  assign T15 = full ^ 1'h1;
  assign full = ptr_match & maybe_full;

  always @(posedge clk) begin
    if (do_enq)
      ram[R2] <= io_enq_bits;
    if(reset) begin
      R2 <= 1'h0;
    end else if(do_enq) begin
      R2 <= T5;
    end
    if(reset) begin
      R6 <= 1'h0;
    end else if(do_deq) begin
      R6 <= T9;
    end
    if(reset) begin
      maybe_full <= 1'h0;
    end else if(T14) begin
      maybe_full <= do_enq;
    end
  end
endmodule

module StdlibSuite_QueueComp_1(input clk, input reset,
    output io_req_ready,
    input  io_req_valid,
    input [7:0] io_req_bits,
    input  io_resp_ready,
    output io_resp_valid,
    output[7:0] io_resp_bits
);

  wire[7:0] Queue_io_deq_bits;
  wire Queue_io_deq_valid;
  wire Queue_io_enq_ready;


  assign io_resp_bits = Queue_io_deq_bits;
  assign io_resp_valid = Queue_io_deq_valid;
  assign io_req_ready = Queue_io_enq_ready;
  Queue Queue(.clk(clk), .reset(reset),
       .io_enq_ready( Queue_io_enq_ready ),
       .io_enq_valid( io_req_valid ),
       .io_enq_bits( io_req_bits ),
       .io_deq_ready( io_resp_ready ),
       .io_deq_valid( Queue_io_deq_valid ),
       .io_deq_bits( Queue_io_deq_bits )
  );
endmodule

