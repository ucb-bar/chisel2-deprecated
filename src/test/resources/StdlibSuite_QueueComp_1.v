module Queue(input clk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input [7:0] io_enq_bits,
    input  io_deq_ready,
    output io_deq_valid,
    output[7:0] io_deq_bits,
    output[1:0] io_count
);

  wire[1:0] T0;
  wire ptr_diff;
  reg  R1;
  wire T15;
  wire T2;
  wire T3;
  wire do_deq;
  reg  R4;
  wire T16;
  wire T5;
  wire T6;
  wire do_enq;
  wire T7;
  wire ptr_match;
  reg  maybe_full;
  wire T17;
  wire T8;
  wire T9;
  wire[7:0] T10;
  reg [7:0] ram [1:0];
  wire[7:0] T11;
  wire T12;
  wire empty;
  wire T13;
  wire T14;
  wire full;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    R1 = {1{$random}};
    R4 = {1{$random}};
    maybe_full = {1{$random}};
    for (initvar = 0; initvar < 2; initvar = initvar+1)
      ram[initvar] = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_count = T0;
  assign T0 = {T7, ptr_diff};
  assign ptr_diff = R4 - R1;
  assign T15 = reset ? 1'h0 : T2;
  assign T2 = do_deq ? T3 : R1;
  assign T3 = R1 + 1'h1;
  assign do_deq = io_deq_ready & io_deq_valid;
  assign T16 = reset ? 1'h0 : T5;
  assign T5 = do_enq ? T6 : R4;
  assign T6 = R4 + 1'h1;
  assign do_enq = io_enq_ready & io_enq_valid;
  assign T7 = maybe_full & ptr_match;
  assign ptr_match = R4 == R1;
  assign T17 = reset ? 1'h0 : T8;
  assign T8 = T9 ? do_enq : maybe_full;
  assign T9 = do_enq != do_deq;
  assign io_deq_bits = T10;
  assign T10 = ram[R1];
  assign io_deq_valid = T12;
  assign T12 = empty ^ 1'h1;
  assign empty = ptr_match & T13;
  assign T13 = maybe_full ^ 1'h1;
  assign io_enq_ready = T14;
  assign T14 = full ^ 1'h1;
  assign full = ptr_match & maybe_full;

  always @(posedge clk) begin
    if(reset) begin
      R1 <= 1'h0;
    end else if(do_deq) begin
      R1 <= T3;
    end
    if(reset) begin
      R4 <= 1'h0;
    end else if(do_enq) begin
      R4 <= T6;
    end
    if(reset) begin
      maybe_full <= 1'h0;
    end else if(T9) begin
      maybe_full <= do_enq;
    end
    if (do_enq)
      ram[R4] <= io_enq_bits;
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
       //.io_count(  )
  );
endmodule

