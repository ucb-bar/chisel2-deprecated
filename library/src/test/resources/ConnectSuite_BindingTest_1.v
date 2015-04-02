module ConnectSuite_CrossingBlock_1(
    input [7:0] io_i1,
    input [7:0] io_i2,
    output[7:0] io_o1,
    output[7:0] io_o2
);

  wire[7:0] T0;


  assign io_o2 = T0;
  assign T0 = io_o1 + io_i2;
  assign io_o1 = io_i1;
endmodule

module ConnectSuite_BindingTestInternal_1(
    input [7:0] io_in1,
    input [7:0] io_in2,
    input [7:0] io_in3,
    input [7:0] io_in4,
    output[7:0] io_out1,
    output[7:0] io_out2,
    output[7:0] io_out3,
    output[7:0] io_out4,
    output[7:0] io_out5,
    output[7:0] io_out6,
    output[7:0] io_out7,
    output[7:0] io_out8,
    output[7:0] io_out9
);

  wire[7:0] T0;
  wire[7:0] T1;
  wire[7:0] T2;
  wire[7:0] cb5_io_o1;
  wire[7:0] cb5_io_o2;
  wire[7:0] cb4_io_o1;
  wire[7:0] cb4_io_o2;
  wire[7:0] cb3_io_o1;
  wire[7:0] cb3_io_o2;
  wire[7:0] cb2_io_o1;
  wire[7:0] cb2_io_o2;
  wire[7:0] cb1_io_o1;
  wire[7:0] cb1_io_o2;


  assign T0 = io_in1 + 8'h1;
  assign io_out9 = io_out7;
  assign io_out8 = cb5_io_o2;
  assign io_out7 = cb4_io_o2;
  assign io_out6 = cb4_io_o1;
  assign io_out5 = T1;
  assign T1 = T2 + io_out4;
  assign T2 = cb3_io_o1 + cb3_io_o2;
  assign io_out4 = cb2_io_o2;
  assign io_out3 = cb2_io_o1;
  assign io_out2 = io_in2;
  assign io_out1 = cb1_io_o1;
  ConnectSuite_CrossingBlock_1 cb5(
       .io_i1( io_in4 ),
       .io_i2( cb5_io_o1 ),
       .io_o1( cb5_io_o1 ),
       .io_o2( cb5_io_o2 )
  );
  ConnectSuite_CrossingBlock_1 cb4(
       .io_i1( io_in3 ),
       .io_i2( io_in3 ),
       .io_o1( cb4_io_o1 ),
       .io_o2( cb4_io_o2 )
  );
  ConnectSuite_CrossingBlock_1 cb3(
       .io_i1( T0 ),
       .io_i2( cb1_io_o2 ),
       .io_o1( cb3_io_o1 ),
       .io_o2( cb3_io_o2 )
  );
  ConnectSuite_CrossingBlock_1 cb2(
       .io_i1( cb1_io_o2 ),
       .io_i2( io_out1 ),
       .io_o1( cb2_io_o1 ),
       .io_o2( cb2_io_o2 )
  );
  ConnectSuite_CrossingBlock_1 cb1(
       .io_i1( io_in1 ),
       .io_i2( io_in2 ),
       .io_o1( cb1_io_o1 ),
       .io_o2( cb1_io_o2 )
  );
endmodule

module ConnectSuite_BindingTest_1(
    input [7:0] io_in1,
    input [7:0] io_in2,
    input [7:0] io_in3,
    input [7:0] io_in4,
    output[7:0] io_out1,
    output[7:0] io_out2,
    output[7:0] io_out3,
    output[7:0] io_out4,
    output[7:0] io_out5,
    output[7:0] io_out6,
    output[7:0] io_out7,
    output[7:0] io_out8,
    output[7:0] io_out9
);

  wire[7:0] myTest_io_out1;
  wire[7:0] myTest_io_out2;
  wire[7:0] myTest_io_out3;
  wire[7:0] myTest_io_out4;
  wire[7:0] myTest_io_out5;
  wire[7:0] myTest_io_out6;
  wire[7:0] myTest_io_out7;
  wire[7:0] myTest_io_out8;
  wire[7:0] myTest_io_out9;


  assign io_out9 = myTest_io_out9;
  assign io_out8 = myTest_io_out8;
  assign io_out7 = myTest_io_out7;
  assign io_out6 = myTest_io_out6;
  assign io_out5 = myTest_io_out5;
  assign io_out4 = myTest_io_out4;
  assign io_out3 = myTest_io_out3;
  assign io_out2 = myTest_io_out2;
  assign io_out1 = myTest_io_out1;
  ConnectSuite_BindingTestInternal_1 myTest(
       .io_in1( io_in1 ),
       .io_in2( io_in2 ),
       .io_in3( io_in3 ),
       .io_in4( io_in4 ),
       .io_out1( myTest_io_out1 ),
       .io_out2( myTest_io_out2 ),
       .io_out3( myTest_io_out3 ),
       .io_out4( myTest_io_out4 ),
       .io_out5( myTest_io_out5 ),
       .io_out6( myTest_io_out6 ),
       .io_out7( myTest_io_out7 ),
       .io_out8( myTest_io_out8 ),
       .io_out9( myTest_io_out9 )
  );
endmodule

