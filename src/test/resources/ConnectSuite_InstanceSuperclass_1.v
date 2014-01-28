module ConnectSuite_A_3(
    input  io_a_in,
    output io_a_out
);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_InstanceSuperclass_1(
    input  io_a_in,
    output io_a_out
);


  assign io_a_out = io_a_in;
  ConnectSuite_A_3 aInBComp(
       .io_a_in( io_a_in )
       //.io_a_out(  )
  );
endmodule

