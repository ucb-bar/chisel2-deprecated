module BusSuite_WriteLowTest_1(
    input [3:0] io_in,
    input  io_en,
    output[3:0] io_out
);

  wire[3:0] T0;
  wire[3:0] myBus;
  wire[3:0] writer_data;
  wire writer_write;


  assign io_out = T0;
  assign T0 = myBus;
  assign myBus = (writer_write) ? writer_data : 4'bzzzz;
  assign writer_data = io_in;
  assign writer_write = io_en;
endmodule

