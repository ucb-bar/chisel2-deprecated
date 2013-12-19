module MultiClockSuite_ClockedSubComp_1(input T0,
    input io_ready,
    output io_valid
);

  reg stored;

  assign io_valid = stored;

  always @(posedge T0) begin
    stored <= io_ready;
  end
endmodule

module MultiClockSuite_Comp_1(input T0,
    input io_data0,
    input io_data1,
    output io_result
);

  wire sub_io_valid;
  wire T1;

  assign io_result = sub_io_valid;
  assign T1 = io_data0 & io_data1;
  MultiClockSuite_ClockedSubComp_1 sub(.T0(T0),
       .io_ready( T1 ),
       .io_valid( sub_io_valid )
  );
endmodule

