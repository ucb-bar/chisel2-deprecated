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
  wire T13;
  wire T3;
  wire T4;
  reg  R5;
  wire T14;
  wire T6;
  wire T7;
  wire do_deq;
  wire T8;
  wire empty;
  wire T9;
  reg  maybe_full;
  wire T15;
  wire T10;
  wire T11;
  wire ptr_match;
  wire T12;
  wire full;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    for (initvar = 0; initvar < 2; initvar = initvar+1)
      ram[initvar] = {1{$random}};
    R2 = {1{$random}};
    R5 = {1{$random}};
    maybe_full = {1{$random}};
  end
`endif

  assign io_deq_bits = T0;
  assign T0 = ram[R5];
  assign do_enq = io_enq_ready & io_enq_valid;
  assign T13 = reset ? 1'h0 : T3;
  assign T3 = do_enq ? T4 : R2;
  assign T4 = R2 + 1'h1;
  assign T14 = reset ? 1'h0 : T6;
  assign T6 = do_deq ? T7 : R5;
  assign T7 = R5 + 1'h1;
  assign do_deq = io_deq_ready & io_deq_valid;
  assign io_deq_valid = T8;
  assign T8 = empty ^ 1'h1;
  assign empty = ptr_match & T9;
  assign T9 = maybe_full ^ 1'h1;
  assign T15 = reset ? 1'h0 : T10;
  assign T10 = T11 ? do_enq : maybe_full;
  assign T11 = do_enq != do_deq;
  assign ptr_match = R2 == R5;
  assign io_enq_ready = T12;
  assign T12 = full ^ 1'h1;
  assign full = ptr_match & maybe_full;

  always @(posedge clk) begin
    if (do_enq)
      ram[R2] <= io_enq_bits;
    if(reset) begin
      R2 <= 1'h0;
    end else if(do_enq) begin
      R2 <= T4;
    end
    if(reset) begin
      R5 <= 1'h0;
    end else if(do_deq) begin
      R5 <= T7;
    end
    if(reset) begin
      maybe_full <= 1'h0;
    end else if(T11) begin
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

  wire Queue_io_enq_ready;
  wire Queue_io_deq_valid;
  wire[7:0] Queue_io_deq_bits;


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

