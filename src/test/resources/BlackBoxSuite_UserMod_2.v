module BlackBoxSuite_UserMod_2(input clk, input usrClk, input reset,
    input [3:0] io_in,
    output[3:0] io_out
);

  reg [3:0] inDelay;
  wire[3:0] T0;
  wire[3:0] userbb_out;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    inDelay = {1{$random}};
  end
// synthesis translate_on
`endif

  assign T0 = reset ? 4'h0 : io_in;
  assign io_out = userbb_out;
  UserClockedBB userbb(.clkIn(usrClk), .rst(reset),
       .in( inDelay ),
       .out( userbb_out )
  );

  always @(posedge clk) begin
    if(reset) begin
      inDelay <= 4'h0;
    end else begin
      inDelay <= io_in;
    end
  end
endmodule

