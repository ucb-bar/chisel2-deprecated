module MultiClockSuite_ClockDec_1(input myClock, input myNewReset,
    input  io_in,
    output io_out
);

  reg  reg_;
  wire T1;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    reg_ = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_out = reg_;
  assign T1 = myNewReset ? 1'h0 : io_in;

  always @(posedge myClock) begin
    if(myNewReset) begin
      reg_ <= 1'h0;
    end else begin
      reg_ <= io_in;
    end
  end
endmodule

