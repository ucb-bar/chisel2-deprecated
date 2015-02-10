module MultiClockSuite_ClockedSubComp_1(input T0,
    input  io_ready,
    output io_valid
);

  reg  stored;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    stored = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_valid = stored;

  always @(posedge T0) begin
    stored <= io_ready;
  end
endmodule

module MultiClockSuite_Comp_1(input T0,
    input  io_data0,
    input  io_data1,
    output io_result
);

  wire T0;
  wire sub_io_valid;


  assign T0 = io_data0 & io_data1;
  assign io_result = sub_io_valid;
  MultiClockSuite_ClockedSubComp_1 sub(.T0(T0),
       .io_ready( T0 ),
       .io_valid( sub_io_valid )
  );
endmodule

