module NameSuite_Block_1(
    input  io_irq,
    output[4:0] io_irq_cause
);



  assign io_irq_cause = 5'h2;
endmodule

module NameSuite_BindSecondComp_1(
    input  io_irq,
    output[5:0] io_irq_cause
);

  wire[5:0] T0;
  wire[4:0] NameSuite_Block_1_io_irq_cause;


  assign io_irq_cause = T0;
  assign T0 = {1'h1, NameSuite_Block_1_io_irq_cause};
  NameSuite_Block_1 NameSuite_Block_1(
       .io_irq( io_irq ),
       .io_irq_cause( NameSuite_Block_1_io_irq_cause )
  );
endmodule

