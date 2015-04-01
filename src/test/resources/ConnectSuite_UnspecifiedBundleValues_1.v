module ConnectSuite_UnspecifiedBundleValues_1(input clk, input reset,
    input  io_in_a,
    input  io_in_b,
    input  io_something,
    output io_out
);

  wire T0;
  wire[103:0] T1;
  wire T2;
  reg  my_reg_b;
  wire T3;
  wire T7;
  wire T4;
  reg  my_reg_a;
  wire T5;
  wire T8;
  wire T6;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    my_reg_b = {1{$random}};
    my_reg_a = {1{$random}};
  end
// synthesis translate_on
`endif

`ifndef SYNTHESIS
// synthesis translate_off
  assign T3 = {1{$random}};
// synthesis translate_on
`endif
  assign T0 = reset ^ 1'h1;
  assign io_out = T2;
  assign T2 = my_reg_a | my_reg_b;
  assign T7 = reset ? T3 : T4;
  assign T4 = io_something ? io_in_b : my_reg_b;
  assign T5 = 1'h0;
  assign T8 = reset ? T5 : T6;
  assign T6 = io_something ? io_in_a : my_reg_a;

  always @(posedge clk) begin
    if(reset) begin
      my_reg_b <= T3;
    end else if(io_something) begin
      my_reg_b <= io_in_b;
    end
    if(reset) begin
      my_reg_a <= T5;
    end else if(io_something) begin
      my_reg_a <= io_in_a;
    end
`ifndef SYNTHESIS
// synthesis translate_off
`ifdef PRINTF_COND
    if (`PRINTF_COND)
`endif
      if (T0)
        $fwrite(32'h80000002, "Hello World!\n");
// synthesis translate_on
`endif
  end
endmodule

