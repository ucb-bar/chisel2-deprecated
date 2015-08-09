module MultiClockSuite_ClockedSubComp_2(input C3, input C4, input reset,
    input  io_ready,
    output io_valid_A,
    output io_valid_B
);

  reg  stored_B;
  wire T1;
  reg  stored_A;
  wire T2;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    stored_B = {1{$random}};
    stored_A = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_valid_B = stored_B;
  assign T1 = reset ? 1'h0 : io_ready;
  assign io_valid_A = stored_A;
  assign T2 = reset ? 1'h0 : io_ready;

  always @(posedge C3) begin
    if(reset) begin
      stored_A <= 1'h0;
    end else begin
      stored_A <= io_ready;
    end
  end
  always @(posedge C4) begin
    if(reset) begin
      stored_B <= 1'h0;
    end else begin
      stored_B <= io_ready;
    end
  end
endmodule

module MultiClockSuite_SingleReset_1(input clk, input C3, input C4, input reset,
    input  io_data,
    output io_result_A,
    output io_result_B
);

  reg  R0;
  reg  R1;
  wire sub_io_valid_A;
  wire sub_io_valid_B;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    R0 = {1{$random}};
    R1 = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_result_B = R0;
  assign io_result_A = R1;
  MultiClockSuite_ClockedSubComp_2 sub(.C3(C3), .C4(C4), .reset(reset), .reset(reset),
       .io_ready( io_data ),
       .io_valid_A( sub_io_valid_A ),
       .io_valid_B( sub_io_valid_B )
  );

  always @(posedge clk) begin
    R0 <= sub_io_valid_B;
    R1 <= sub_io_valid_A;
  end
endmodule

