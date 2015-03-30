module MultiClockSuite_TestMultiClock2_1_TestMultiClock2_subsub(input clkB,
    input  io_in,
    output io_out
);

  reg  r1_onSignal;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    r1_onSignal = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_out = r1_onSignal;

  always @(posedge clkB) begin
    r1_onSignal <= io_in;
  end
endmodule

module MultiClockSuite_TestMultiClock2_1_TestMultiClock2_sub(input clkB,
    input  io_in,
    output io_out
);

  wire sub_io_out;


  assign io_out = sub_io_out;
  MultiClockSuite_TestMultiClock2_1_TestMultiClock2_subsub sub(.clkB(clkB),
       .io_in( io_in ),
       .io_out( sub_io_out )
  );
endmodule

module MultiClockSuite_TestMultiClock2_1(input clkB,
    input  io_in,
    output io_out
);

  wire sub_io_out;


  assign io_out = sub_io_out;
  MultiClockSuite_TestMultiClock2_1_TestMultiClock2_sub sub(.clkB(clkB),
       .io_in( io_in ),
       .io_out( sub_io_out )
  );
endmodule

