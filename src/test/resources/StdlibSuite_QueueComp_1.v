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
  wire[7:0] T2;
  wire do_enq;
  wire T3;
  wire do_flow;
  wire T4;
  reg[0:0] enq_ptr;
  wire T5;
  reg[0:0] deq_ptr;
  wire do_deq;
  wire T6;
  wire T7;
  wire T8;
  wire T9;
  wire empty;
  wire T10;
  reg[0:0] maybe_full;
  wire T11;
  wire ptr_match;
  wire T12;
  wire full;

  assign io_deq_bits = T0;
  assign T0 = ram[deq_ptr];
  assign T2 = io_enq_bits;
  assign do_enq = T4 && T3;
  assign T3 = ! do_flow;
  assign do_flow = 1'h0;
  assign T4 = io_enq_ready && io_enq_valid;
  assign T5 = enq_ptr + 1'h1;
  assign do_deq = T7 && T6;
  assign T6 = ! do_flow;
  assign T7 = io_deq_ready && io_deq_valid;
  assign T8 = deq_ptr + 1'h1;
  assign io_deq_valid = T9;
  assign T9 = ! empty;
  assign empty = ptr_match && T10;
  assign T10 = ! maybe_full;
  assign T11 = do_enq != do_deq;
  assign ptr_match = enq_ptr == deq_ptr;
  assign io_enq_ready = T12;
  assign T12 = ! full;
  assign full = ptr_match && maybe_full;

  always @(posedge clk) begin
    if (do_enq)
      ram[enq_ptr] <= T2;
    if(reset) begin
      enq_ptr <= 1'h0;
    end else if(do_enq) begin
      enq_ptr <= T5;
    end
    if(reset) begin
      deq_ptr <= 1'h0;
    end else if(do_deq) begin
      deq_ptr <= T8;
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

