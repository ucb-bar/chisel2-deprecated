module BusSuite_WriteMultiTest_1(
    input [129:0] io_in,
    input  io_en,
    output[129:0] io_out
);

  wire[129:0] T0;
  wire[129:0] myBus;
  wire[129:0] writer_data;
  wire writer_write;


  assign io_out = T0;
  assign T0 = myBus;
  assign myBus = (writer_write) ? writer_data : 130'bzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;
  assign writer_data = io_in;
  assign writer_write = io_en;
endmodule

