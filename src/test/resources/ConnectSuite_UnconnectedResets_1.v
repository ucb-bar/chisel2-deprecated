module ConnectSuite_SubModule_1(input clk, input reset,
    input  io_in,
    output io_out
);

  reg  r;
  wire T0;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    r = {1{$random}};
  end
`endif

  assign io_out = r;
  assign T0 = reset ? 1'h1 : io_in;

  always @(posedge clk) begin
    if(reset) begin
      r <= 1'h1;
    end else begin
      r <= io_in;
    end
  end
endmodule

module ConnectSuite_UnconnectedResets_1(input clk, input reset,
    input  io_in,
    output io_out
);

  reg  regs_2;
  reg  regs_1;
  reg  regs_0;
  wire sub_io_out;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    regs_2 = {1{$random}};
    regs_1 = {1{$random}};
    regs_0 = {1{$random}};
  end
`endif

  assign io_out = sub_io_out;
  ConnectSuite_SubModule_1 sub(.clk(clk), .reset(regs_2),
       .io_in( io_in ),
       .io_out( sub_io_out )
  );

  always @(posedge clk) begin
    regs_2 <= regs_1;
    regs_1 <= regs_0;
    regs_0 <= reset;
  end
endmodule

