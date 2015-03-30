module NameSuite_Comp_1(
    input  io_in_ren,
    output io_out_ren
);



  assign io_out_ren = io_in_ren;
endmodule

module NameSuite_BindThirdComp_1(
    input  io_in_ren,
    output io_result
);

  wire T0;
  wire T1;
  wire T2;
  wire NameSuite_Comp_1_io_out_ren;
  wire NameSuite_Comp_1_1_io_out_ren;
  wire NameSuite_Comp_1_2_io_out_ren;
  wire NameSuite_Comp_1_3_io_out_ren;


  assign io_result = T0;
  assign T0 = T1 | NameSuite_Comp_1_3_io_out_ren;
  assign T1 = T2 | NameSuite_Comp_1_2_io_out_ren;
  assign T2 = NameSuite_Comp_1_io_out_ren | NameSuite_Comp_1_1_io_out_ren;
  NameSuite_Comp_1 NameSuite_Comp_1(
       .io_in_ren( io_in_ren ),
       .io_out_ren( NameSuite_Comp_1_io_out_ren )
  );
  NameSuite_Comp_1 NameSuite_Comp_1_1(
       .io_in_ren( NameSuite_Comp_1_io_out_ren ),
       .io_out_ren( NameSuite_Comp_1_1_io_out_ren )
  );
  NameSuite_Comp_1 NameSuite_Comp_1_2(
       .io_in_ren( NameSuite_Comp_1_1_io_out_ren ),
       .io_out_ren( NameSuite_Comp_1_2_io_out_ren )
  );
  NameSuite_Comp_1 NameSuite_Comp_1_3(
       .io_in_ren( NameSuite_Comp_1_2_io_out_ren ),
       .io_out_ren( NameSuite_Comp_1_3_io_out_ren )
  );
endmodule

