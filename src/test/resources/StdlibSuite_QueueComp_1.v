module Queue(input clk, input reset,
    output io_enq_ready,
    input io_enq_valid,
    input [7:0] io_enq_bits,
    input io_deq_ready,
    output io_deq_valid,
    output [7:0] io_deq_bits
);

  wire T0;
  wire full;
  wire ptr_match;
  reg enq_ptr;
  wire T1;
  wire do_enq;
  wire T2;
  wire T3;
  wire do_flow;
  wire T4;
  reg deq_ptr;
  wire T5;
  wire do_deq;
  wire T6;
  wire T7;
  wire empty;
  wire T8;
  reg maybe_full;
  wire T9;
  wire T10;
  wire T11;
  wire T12;
  wire [7:0] T13;
  reg [7:0] ram [1:0];
  wire [7:0] T14;
  wire [7:0] T15;

  assign io_enq_ready = T0;
  assign T0 = ! full;
  assign full = ptr_match && maybe_full;
  assign ptr_match = enq_ptr == deq_ptr;
  assign T1 = do_enq ? T4 : enq_ptr;
  assign do_enq = T2 && T3;
  assign T2 = io_enq_ready && io_enq_valid;
  assign T3 = ! do_flow;
  assign do_flow = 1'h0/* 0*/;
  assign T4 = enq_ptr + 1'h1/* 1*/;
  assign T5 = do_deq ? T12 : deq_ptr;
  assign do_deq = T6 && T11;
  assign T6 = io_deq_ready && io_deq_valid;
  assign io_deq_valid = T7;
  assign T7 = ! empty;
  assign empty = ptr_match && T8;
  assign T8 = ! maybe_full;
  assign T9 = T10 ? do_enq : maybe_full;
  assign T10 = do_enq != do_deq;
  assign T11 = ! do_flow;
  assign T12 = deq_ptr + 1'h1/* 1*/;
  assign io_deq_bits = T13;
  assign T15 = io_enq_bits;

  always @(posedge clk) begin
    if(reset) begin
      enq_ptr <= 1'h0/* 0*/;
    end else if(do_enq) begin
      enq_ptr <= T1;
    end
    if(reset) begin
      deq_ptr <= 1'h0/* 0*/;
    end else if(do_deq) begin
      deq_ptr <= T5;
    end
    if(reset) begin
      maybe_full <= 1'h0/* 0*/;
    end else if(T10) begin
      maybe_full <= T9;
    end
    if (do_enq)
      ram[enq_ptr] <= T15;
  end
endmodule

module StdlibSuite_QueueComp_1(input clk, input reset,
    output io_req_ready,
    input io_req_valid,
    input [7:0] io_req_bits,
    input io_resp_ready,
    output io_resp_valid,
    output [7:0] io_resp_bits
);

  wire Queue_io_enq_ready;
  wire Queue_io_deq_valid;
  wire [7:0] Queue_io_deq_bits;

  assign io_req_ready = Queue_io_enq_ready;
  assign io_resp_valid = Queue_io_deq_valid;
  assign io_resp_bits = Queue_io_deq_bits;
  Queue Queue(.clk(clk), .reset(reset),
       .io_enq_ready( Queue_io_enq_ready ),
       .io_enq_valid( io_req_valid ),
       .io_enq_bits( io_req_bits ),
       .io_deq_ready( io_resp_ready ),
       .io_deq_valid( Queue_io_deq_valid ),
       .io_deq_bits( Queue_io_deq_bits )
  );
endmodule

