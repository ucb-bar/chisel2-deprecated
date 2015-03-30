module StdlibSuite_CatComp_1(
    input [7:0] io_x,
    input [7:0] io_y,
    output[15:0] io_z
);

  wire[15:0] T0;


  assign io_z = T0;
  assign T0 = {io_x, io_y};
endmodule

