module NameSuite_CompBlock_1_0(input reset,
    input  io_valid,
    output io_replay
);



  assign io_replay = 1'h0;
endmodule

module NameSuite_CompBlock_1_1(input reset,
    input  io_valid,
    output io_replay
);



  assign io_replay = io_valid;
endmodule

module NameSuite_VariationComp_1(input reset,
    input  io_valid,
    output io_replay
);

  wire T0;
  wire T1;
  wire block_0_io_replay;
  wire block_1_io_replay;
  wire block_2_io_replay;


  assign io_replay = T0;
  assign T0 = T1 & block_2_io_replay;
  assign T1 = block_0_io_replay & block_1_io_replay;
  NameSuite_CompBlock_1_0 block_0(.reset(reset),
       .io_valid( io_valid ),
       .io_replay( block_0_io_replay )
  );
  NameSuite_CompBlock_1_0 block_1(.reset(reset),
       .io_valid( io_valid ),
       .io_replay( block_1_io_replay )
  );
  NameSuite_CompBlock_1_1 block_2(.reset(reset),
       .io_valid( io_valid ),
       .io_replay( block_2_io_replay )
  );
endmodule

