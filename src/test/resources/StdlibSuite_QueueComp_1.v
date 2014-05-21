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
  wire T2;
  wire do_flow;
  wire T3;
  reg  enq_ptr;
  wire T4;
  wire T5;
  wire T6;
  reg  deq_ptr;
  wire T7;
  wire T8;
  wire T9;
  wire do_deq;
  wire T10;
  wire T11;
  wire T12;
  wire empty;
  wire T13;
  reg  maybe_full;
  wire T14;
  wire T15;
  wire T16;
  wire ptr_match;
  wire T17;
  wire full;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    for (initvar = 0; initvar < 2; initvar = initvar+1)
      ram[initvar] = {1{$random}};
    enq_ptr = {1{$random}};
    deq_ptr = {1{$random}};
    maybe_full = {1{$random}};
  end
`endif

  assign io_deq_bits = T0;
  assign T0 = ram[deq_ptr];
  assign do_enq = T3 & T2;
  assign T2 = do_flow == 1'h0;
  assign do_flow = 1'h0;
  assign T3 = io_enq_ready & io_enq_valid;
  assign T4 = reset ? 1'h0 : T5;
  assign T5 = do_enq ? T6 : enq_ptr;
  assign T6 = enq_ptr + 1'h1;
  assign T7 = reset ? 1'h0 : T8;
  assign T8 = do_deq ? T9 : deq_ptr;
  assign T9 = deq_ptr + 1'h1;
  assign do_deq = T11 & T10;
  assign T10 = do_flow == 1'h0;
  assign T11 = io_deq_ready & io_deq_valid;
  assign io_deq_valid = T12;
  assign T12 = empty == 1'h0;
  assign empty = ptr_match & T13;
  assign T13 = maybe_full == 1'h0;
  assign T14 = reset ? 1'h0 : T15;
  assign T15 = T16 ? do_enq : maybe_full;
  assign T16 = do_enq != do_deq;
  assign ptr_match = enq_ptr == deq_ptr;
  assign io_enq_ready = T17;
  assign T17 = full == 1'h0;
  assign full = ptr_match & maybe_full;

  always @(posedge clk) begin
    if (do_enq)
      ram[enq_ptr] <= io_enq_bits;
    if(reset) begin
      enq_ptr <= 1'h0;
    end else if(do_enq) begin
      enq_ptr <= T6;
    end
    if(reset) begin
      deq_ptr <= 1'h0;
    end else if(do_deq) begin
      deq_ptr <= T9;
    end
    if(reset) begin
      maybe_full <= 1'h0;
    end else if(T16) begin
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

