module NameSuite_VecSecondComp_1(input clk,
    output io_requestor_3_req_ready,
    input  io_requestor_3_req_valid,
    input  io_requestor_3_req_bits_ready,
    output io_requestor_2_req_ready,
    input  io_requestor_2_req_valid,
    input  io_requestor_2_req_bits_ready,
    output io_requestor_1_req_ready,
    input  io_requestor_1_req_valid,
    input  io_requestor_1_req_bits_ready,
    output io_requestor_0_req_ready,
    input  io_requestor_0_req_valid,
    input  io_requestor_0_req_bits_ready,
    output io_mem
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;
  reg  r_valid_0;
  reg  r_valid_1;
  reg  r_valid_2;
  reg  r_valid_3;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    r_valid_0 = {1{$random}};
    r_valid_1 = {1{$random}};
    r_valid_2 = {1{$random}};
    r_valid_3 = {1{$random}};
  end
// synthesis translate_on
`endif

`ifndef SYNTHESIS
// synthesis translate_off
  assign io_requestor_0_req_ready = {1{$random}};
  assign io_requestor_1_req_ready = {1{$random}};
  assign io_requestor_2_req_ready = {1{$random}};
  assign io_requestor_3_req_ready = {1{$random}};
// synthesis translate_on
`endif
  assign io_mem = T0;
  assign T0 = r_valid_3 ? io_requestor_3_req_ready : T1;
  assign T1 = r_valid_2 ? io_requestor_2_req_ready : T2;
  assign T2 = r_valid_1 ? io_requestor_1_req_ready : T3;
  assign T3 = r_valid_0 ? io_requestor_0_req_ready : io_requestor_0_req_ready;

  always @(posedge clk) begin
    r_valid_0 <= io_requestor_0_req_ready;
    r_valid_1 <= io_requestor_1_req_ready;
    r_valid_2 <= io_requestor_2_req_ready;
    r_valid_3 <= io_requestor_3_req_ready;
  end
endmodule

