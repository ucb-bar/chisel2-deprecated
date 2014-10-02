module StdlibSuite_RemUS_1(
    input [31:0] io_x,
    input [31:0] io_y,
    output[31:0] io_z
);

  wire[31:0] T0;
  wire[32:0] T1;
  wire[32:0] T2;


  assign io_z = T0;
  assign T0 = $signed(T1) % $signed(io_y);
  assign T1 = T2;
  assign T2 = {1'h0, io_x};
endmodule

