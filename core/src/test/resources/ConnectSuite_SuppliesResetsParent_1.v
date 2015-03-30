module Queue(input clk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input  io_enq_bits,
    input  io_deq_ready,
    output io_deq_valid,
    output io_deq_bits,
    output io_count
);

  wire T0;
  wire[2:0] T1;
  reg[0:0] maybe_full;
  wire T2;
  wire do_deq;
  wire T3;
  wire do_flow;
  wire T4;
  wire do_enq;
  wire T5;
  wire T6;
  wire T7;
  reg [0:0] ram [0:0];
  wire T8;
  wire T9;
  wire T10;
  wire empty;
  wire T11;

  assign io_count = T0;
  assign T0 = T1[1'h0/* 0*/:1'h0/* 0*/];
  assign T1 = {maybe_full, 2'h0/* 0*/};
  assign T2 = do_enq != do_deq;
  assign do_deq = T4 && T3;
  assign T3 = ! do_flow;
  assign do_flow = 1'h0/* 0*/;
  assign T4 = io_deq_ready && io_deq_valid;
  assign do_enq = T6 && T5;
  assign T5 = ! do_flow;
  assign T6 = io_enq_ready && io_enq_valid;
  assign io_deq_bits = T7;
  assign T7 = ram[1'h0/* 0*/];
  assign T9 = io_enq_bits;
  assign io_deq_valid = T10;
  assign T10 = ! empty;
  assign empty = ! maybe_full;
  assign io_enq_ready = T11;
  assign T11 = ! maybe_full;

  always @(posedge clk) begin
    if(reset) begin
      maybe_full <= 1'h0/* 0*/;
    end else if(T2) begin
      maybe_full <= do_enq;
    end
    if (do_enq)
      ram[1'h0/* 0*/] <= T9;
  end
endmodule

module ConnectSuite_UsesReset_2(input clk, input reset,
    input  io_in,
    output io_out
);

  wire T0;
  wire q_io_deq_bits;

  assign io_out = T0;
  assign T0 = q_io_deq_bits || reset;
  Queue q(.clk(clk), .reset(reset),
       //.io_enq_ready(  )
       .io_enq_valid( 1'h1/* 1*/ ),
       .io_enq_bits( io_in ),
       .io_deq_ready( 1'h1/* 1*/ ),
       //.io_deq_valid(  )
       .io_deq_bits( q_io_deq_bits )
       //.io_count(  )
  );
endmodule

module ConnectSuite_SuppliesResets_1(input clk, input reset,
    input  io_in,
    output io_out
);

  reg[0:0] R0;
  reg[0:0] delayed;
  wire T1;
  wire T2;
  wire a2_io_out;
  wire T3;
  wire a1_io_out;
  wire a0_io_out;

  assign io_out = T1;
  assign T1 = T2 || delayed;
  assign T2 = T3 || a2_io_out;
  assign T3 = a0_io_out || a1_io_out;
  ConnectSuite_UsesReset_2 a0(.clk(clk), .reset(reset),
       .io_in( io_in ),
       .io_out( a0_io_out )
  );
  ConnectSuite_UsesReset_2 a1(.clk(clk), .reset(delayed),
       .io_in( io_in ),
       .io_out( a1_io_out )
  );
  ConnectSuite_UsesReset_2 a2(.clk(clk), .reset(R0),
       .io_in( io_in ),
       .io_out( a2_io_out )
  );

  always @(posedge clk) begin
    R0 <= reset;
    delayed <= reset;
  end
endmodule

module ConnectSuite_SuppliesResetsParent_1(input clk, input reset,
    input  io_in,
    output io_out
);

  wire srs_io_out;

  assign io_out = srs_io_out;
  ConnectSuite_SuppliesResets_1 srs(.clk(clk), .reset(reset),
       .io_in( io_in ),
       .io_out( srs_io_out )
  );
endmodule

