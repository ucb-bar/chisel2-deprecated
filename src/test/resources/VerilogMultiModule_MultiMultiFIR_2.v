module VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection(
    output io_in_ready,
    input  io_in_valid,
    input [31:0] io_in_bits,
    input  io_out_ready,
    output io_out_valid,
    output[31:0] io_out_bits
);

  wire[31:0] T0;


  assign io_out_bits = T0;
  assign T0 = io_in_bits f* 32'h40000000;
  assign io_out_valid = io_in_valid;
  assign io_in_ready = io_out_ready;
endmodule

module Queue(input clk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input [31:0] io_enq_bits,
    input  io_deq_ready,
    output io_deq_valid,
    output[31:0] io_deq_bits,
    output[2:0] io_count
);

  wire[2:0] T0;
  wire[1:0] ptr_diff;
  reg [1:0] R1;
  wire[1:0] T16;
  wire[1:0] T2;
  wire[1:0] T3;
  wire do_deq;
  reg [1:0] R4;
  wire[1:0] T17;
  wire[1:0] T5;
  wire[1:0] T6;
  wire do_enq;
  wire T7;
  wire ptr_match;
  reg  maybe_full;
  wire T18;
  wire T8;
  wire T9;
  wire[31:0] T10;
  reg [31:0] ram [3:0];
  wire[31:0] T11;
  wire[31:0] T12;
  wire T13;
  wire empty;
  wire T14;
  wire T15;
  wire full;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    R1 = {1{$random}};
    R4 = {1{$random}};
    maybe_full = {1{$random}};
    for (initvar = 0; initvar < 4; initvar = initvar+1)
      ram[initvar] = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_count = T0;
  assign T0 = {T7, ptr_diff};
  assign ptr_diff = R4 - R1;
  assign T16 = reset ? 2'h0 : T2;
  assign T2 = do_deq ? T3 : R1;
  assign T3 = R1 + 2'h1;
  assign do_deq = io_deq_ready & io_deq_valid;
  assign T17 = reset ? 2'h0 : T5;
  assign T5 = do_enq ? T6 : R4;
  assign T6 = R4 + 2'h1;
  assign do_enq = io_enq_ready & io_enq_valid;
  assign T7 = maybe_full & ptr_match;
  assign ptr_match = R4 == R1;
  assign T18 = reset ? 1'h0 : T8;
  assign T8 = T9 ? do_enq : maybe_full;
  assign T9 = do_enq != do_deq;
  assign io_deq_bits = T10;
  assign T10 = ram[R1];
  assign T12 = io_enq_bits;
  assign io_deq_valid = T13;
  assign T13 = empty ^ 1'h1;
  assign empty = ptr_match & T14;
  assign T14 = maybe_full ^ 1'h1;
  assign io_enq_ready = T15;
  assign T15 = full ^ 1'h1;
  assign full = ptr_match & maybe_full;

  always @(posedge clk) begin
    if(reset) begin
      R1 <= 2'h0;
    end else if(do_deq) begin
      R1 <= T3;
    end
    if(reset) begin
      R4 <= 2'h0;
    end else if(do_enq) begin
      R4 <= T6;
    end
    if(reset) begin
      maybe_full <= 1'h0;
    end else if(T9) begin
      maybe_full <= do_enq;
    end
    if (do_enq)
      ram[R4] <= T12;
  end
endmodule

module VerilogMultiModule_MultiMultiFIR_2_MultiFIR(input clk, input reset,
    output io_in_ready,
    input  io_in_valid,
    input [31:0] io_in_bits,
    input  io_out_ready,
    output io_out_valid,
    output[31:0] io_out_bits
);

  wire VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_io_in_ready;
  wire VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_io_out_valid;
  wire[31:0] VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_io_out_bits;
  wire Queue_io_enq_ready;
  wire Queue_io_deq_valid;
  wire[31:0] Queue_io_deq_bits;
  wire VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1_io_in_ready;
  wire VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1_io_out_valid;
  wire[31:0] VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1_io_out_bits;
  wire Queue_1_io_enq_ready;
  wire Queue_1_io_deq_valid;
  wire[31:0] Queue_1_io_deq_bits;
  wire VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2_io_in_ready;
  wire VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2_io_out_valid;
  wire[31:0] VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2_io_out_bits;


  assign io_out_bits = VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2_io_out_bits;
  assign io_out_valid = VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2_io_out_valid;
  assign io_in_ready = VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_io_in_ready;
  VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection(
       .io_in_ready( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_io_in_ready ),
       .io_in_valid( io_in_valid ),
       .io_in_bits( io_in_bits ),
       .io_out_ready( Queue_io_enq_ready ),
       .io_out_valid( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_io_out_valid ),
       .io_out_bits( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_io_out_bits )
  );
  Queue Queue(.clk(clk), .reset(reset),
       .io_enq_ready( Queue_io_enq_ready ),
       .io_enq_valid( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_io_out_valid ),
       .io_enq_bits( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_io_out_bits ),
       .io_deq_ready( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1_io_in_ready ),
       .io_deq_valid( Queue_io_deq_valid ),
       .io_deq_bits( Queue_io_deq_bits )
       //.io_count(  )
  );
  VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1(
       .io_in_ready( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1_io_in_ready ),
       .io_in_valid( Queue_io_deq_valid ),
       .io_in_bits( Queue_io_deq_bits ),
       .io_out_ready( Queue_1_io_enq_ready ),
       .io_out_valid( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1_io_out_valid ),
       .io_out_bits( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1_io_out_bits )
  );
  Queue Queue_1(.clk(clk), .reset(reset),
       .io_enq_ready( Queue_1_io_enq_ready ),
       .io_enq_valid( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1_io_out_valid ),
       .io_enq_bits( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_1_io_out_bits ),
       .io_deq_ready( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2_io_in_ready ),
       .io_deq_valid( Queue_1_io_deq_valid ),
       .io_deq_bits( Queue_1_io_deq_bits )
       //.io_count(  )
  );
  VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2(
       .io_in_ready( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2_io_in_ready ),
       .io_in_valid( Queue_1_io_deq_valid ),
       .io_in_bits( Queue_1_io_deq_bits ),
       .io_out_ready( io_out_ready ),
       .io_out_valid( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2_io_out_valid ),
       .io_out_bits( VerilogMultiModule_MultiMultiFIR_2_MultiFIR_FilterSection_2_io_out_bits )
  );
endmodule

module VerilogMultiModule_MultiMultiFIR_2(input clk, input reset,
    output io_in_ready,
    input  io_in_valid,
    input [31:0] io_in_bits,
    input  io_out_ready,
    output io_out_valid,
    output[31:0] io_out_bits
);

  wire m1_io_out_valid;
  wire[31:0] m1_io_out_bits;
  wire m2_io_in_ready;


`ifndef SYNTHESIS
// synthesis translate_off
//  assign io_out_bits = {1{$random}};
//  assign io_out_valid = {1{$random}};
//  assign io_in_ready = {1{$random}};
// synthesis translate_on
`endif
  VerilogMultiModule_MultiMultiFIR_2_MultiFIR m1(.clk(clk), .reset(reset),
       //.io_in_ready(  )
       //.io_in_valid(  )
       //.io_in_bits(  )
       .io_out_ready( m2_io_in_ready ),
       .io_out_valid( m1_io_out_valid ),
       .io_out_bits( m1_io_out_bits )
  );
`ifndef SYNTHESIS
// synthesis translate_off
    assign m1.io_in_valid = {1{$random}};
    assign m1.io_in_bits = {1{$random}};
// synthesis translate_on
`endif
  VerilogMultiModule_MultiMultiFIR_2_MultiFIR m2(.clk(clk), .reset(reset),
       .io_in_ready( m2_io_in_ready ),
       .io_in_valid( m1_io_out_valid ),
       .io_in_bits( m1_io_out_bits )
       //.io_out_ready(  )
       //.io_out_valid(  )
       //.io_out_bits(  )
  );
`ifndef SYNTHESIS
// synthesis translate_off
    assign m2.io_out_ready = {1{$random}};
// synthesis translate_on
`endif
endmodule

