module StdlibSuite_AssignBundleComp_1(
    input [1:0] io_in_v_1,
    input [1:0] io_in_v_0,
    output[1:0] io_out_v_1,
    output[1:0] io_out_v_0
);



  assign io_out_v_0 = io_in_v_0;
  assign io_out_v_1 = io_in_v_1;
endmodule

