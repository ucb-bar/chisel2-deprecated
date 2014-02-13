module NameSuite_ListLookupsComp_1(
    input [31:0] io_inst,
    output io_sigs_valid
);

  wire T0;
  wire T1;
  wire T2;
  wire[2:0] T3;
  wire[2:0] T4;
  wire T5;
  wire T6;
  wire T7;
  wire T8;
  wire T9;

  assign io_sigs_valid = T0;
  assign T0 = T1;
  assign T1 = T8 ^ T2;
  assign T2 = T3[2'h2:2'h2];
  assign T3 = T7 ? 3'h4 : T4;
  assign T4 = {2'h0, T5};
  assign T5 = T6;
  assign T6 = io_inst == 32'h257b;
  assign T7 = io_inst == 32'h277b;
  assign T8 = T7 ? 1'h1 : T9;
  assign T9 = T6 ? 1'h0 : 1'h1;
endmodule

