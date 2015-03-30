module ConnectSuite_UsesShim_1(
    output io_in_ready,
    input  io_in_valid,
    input  io_in_bits,
    input  io_out_ready,
    output io_out_valid,
    output io_out_bits
);

  wire s_bits;
  wire T0;
  wire s_valid;
  wire s_ready;

  assign io_out_bits = s_bits;
  assign s_bits = T0;
  assign T0 = io_in_bits + 1'h1;
  assign io_out_valid = s_valid;
  assign s_valid = io_in_valid;
  assign io_in_ready = s_ready;
  assign s_ready = io_out_ready;
endmodule

module ConnectSuite_UsesShimParent_1(
    output io_in_ready,
    input  io_in_valid,
    input  io_in_bits,
    input  io_out_ready,
    output io_out_valid,
    output io_out_bits
);

  wire us_io_out_bits;
  wire us_io_out_valid;
  wire us_io_in_ready;

  assign io_out_bits = us_io_out_bits;
  assign io_out_valid = us_io_out_valid;
  assign io_in_ready = us_io_in_ready;
  ConnectSuite_UsesShim_1 us(
       .io_in_ready( us_io_in_ready ),
       .io_in_valid( io_in_valid ),
       .io_in_bits( io_in_bits ),
       .io_out_ready( io_out_ready ),
       .io_out_valid( us_io_out_valid ),
       .io_out_bits( us_io_out_bits )
  );
endmodule

