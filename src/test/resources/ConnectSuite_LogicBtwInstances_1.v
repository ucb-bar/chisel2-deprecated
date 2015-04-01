module ConnectSuite_A_2(
    input  io_a_in,
    output io_a_out
);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_LogicBtwInstances_1(input clk,
    input  io_b_in,
    output io_b_out
);

  wire T0;
  reg[0:0] x;
  wire T1;
  wire a2_io_a_out;
  wire a1_io_a_out;

  assign io_b_out = T0;
  assign T0 = T1 | x;
  assign T1 = a1_io_a_out | a2_io_a_out;
  ConnectSuite_A_2 a1(
       .io_a_in( io_b_in ),
       .io_a_out( a1_io_a_out )
  );
  ConnectSuite_A_2 a2(
       .io_a_in( io_b_in ),
       .io_a_out( a2_io_a_out )
  );

  always @(posedge clk) begin
    x <= io_b_in;
  end
endmodule

