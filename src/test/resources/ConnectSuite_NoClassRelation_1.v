module ConnectSuite_A_1(
    input  io_a_in,
    output io_a_out
);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_B_1(
    input  io_b_in,
    output io_b_out
);

  wire aComp_io_a_out;

  assign io_b_out = aComp_io_a_out;
  ConnectSuite_A_1 aComp(
       .io_a_in( io_b_in ),
       .io_a_out( aComp_io_a_out )
  );
endmodule

module ConnectSuite_NoClassRelation_1(
    input  io_c_in,
    output io_c_out
);

  wire aComp_io_b_out;

  assign io_c_out = aComp_io_b_out;
  ConnectSuite_B_1 aComp(
       .io_b_in( io_c_in ),
       .io_b_out( aComp_io_b_out )
  );
endmodule

