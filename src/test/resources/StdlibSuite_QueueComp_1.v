module Queue(input clk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input [7:0] io_enq_bits,
    input  io_deq_ready,
    output io_deq_valid,
    output[7:0] io_deq_bits
    //output[1:0] io_count
);

  wire[7:0] T0;
  reg [7:0] ram [1:0];
  wire[7:0] T1;
  wire T2;
  reg  R3;
  wire T17;
  wire T4;
  wire T5;
  reg  R6;
  wire T18;
  wire T7;
  wire T8;
  wire T9;
  wire T10;
  wire T11;
  wire T12;
  wire T13;
  reg  R14;
  wire T19;
  wire T15;
  wire T16;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    for (initvar = 0; initvar < 2; initvar = initvar+1)
      ram[initvar] = {1{$random}};
    R3 = {1{$random}};
    R6 = {1{$random}};
    R14 = {1{$random}};
  end
// synthesis translate_on
`endif

`ifndef SYNTHESIS
// synthesis translate_off
//  assign io_count = {1{$random}};
// synthesis translate_on
`endif
  assign io_deq_bits = T0;
  assign T0 = ram[R6];
  assign T2 = io_enq_ready & io_enq_valid;
  assign T17 = reset ? 1'h0 : T4;
  assign T4 = T2 ? T5 : R3;
  assign T5 = R3 + 1'h1;
  assign T18 = reset ? 1'h0 : T7;
  assign T7 = T9 ? T8 : R6;
  assign T8 = R6 + 1'h1;
  assign T9 = io_deq_ready & io_deq_valid;
  assign io_deq_valid = T10;
  assign T10 = T11 ^ 1'h1;
  assign T11 = R3 == R6;
  assign io_enq_ready = T12;
  assign T12 = T13 ^ 1'h1;
  assign T13 = R14 == R6;
  assign T19 = reset ? 1'h1 : T15;
  assign T15 = T2 ? T16 : R14;
  assign T16 = R14 + 1'h1;

  always @(posedge clk) begin
    if (T2)
      ram[R3] <= io_enq_bits;
    if(reset) begin
      R3 <= 1'h0;
    end else if(T2) begin
      R3 <= T5;
    end
    if(reset) begin
      R6 <= 1'h0;
    end else if(T9) begin
      R6 <= T8;
    end
    if(reset) begin
      R14 <= 1'h1;
    end else if(T2) begin
      R14 <= T16;
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
       //.io_count(  )
  );
endmodule

