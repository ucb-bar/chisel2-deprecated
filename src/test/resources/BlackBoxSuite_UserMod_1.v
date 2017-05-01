module BlackBoxSuite_UserMod_1(
    input [3:0] io_in,
    output[3:0] io_out
);

  wire[3:0] userbb_out;


  assign io_out = userbb_out;
  UserBB userbb(
       .in( io_in ),
       .out( userbb_out )
  );
endmodule

