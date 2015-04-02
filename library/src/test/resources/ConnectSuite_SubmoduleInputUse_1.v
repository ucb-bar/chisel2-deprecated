module ConnectSuite_PassThrough_1(
    input [7:0] io_ptin,
    output[7:0] io_ptout
);



  assign io_ptout = io_ptin;
endmodule

module ConnectSuite_SubmoduleInputUse_1(
    input [7:0] io_in,
    output[7:0] io_out1,
    output[7:0] io_out2a,
    output[7:0] io_out2b,
    output[7:0] io_out3
);

  wire[7:0] pt3_io_ptout;
  wire[7:0] pt2b_io_ptout;
  wire[7:0] pt2a_io_ptout;
  wire[7:0] pt1_io_ptout;


  assign io_out3 = pt3_io_ptout;
  assign io_out2b = pt2b_io_ptout;
  assign io_out2a = pt2a_io_ptout;
  assign io_out1 = pt1_io_ptout;
  ConnectSuite_PassThrough_1 pt3(
       .io_ptin( io_out2b ),
       .io_ptout( pt3_io_ptout )
  );
  ConnectSuite_PassThrough_1 pt2b(
       .io_ptin( pt1_io_ptout ),
       .io_ptout( pt2b_io_ptout )
  );
  ConnectSuite_PassThrough_1 pt2a(
       .io_ptin( pt1_io_ptout ),
       .io_ptout( pt2a_io_ptout )
  );
  ConnectSuite_PassThrough_1 pt1(
       .io_ptin( io_in ),
       .io_ptout( pt1_io_ptout )
  );
endmodule

