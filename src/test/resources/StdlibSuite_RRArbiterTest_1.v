module StdlibSuite_RRArbiterTest_1(input clk, input reset,
    output io_in_0_ready,
    input  io_in_0_valid,
    input [7:0] io_in_0_bits,
    output io_in_1_ready,
    input  io_in_1_valid,
    input [7:0] io_in_1_bits,
    output io_in_2_ready,
    input  io_in_2_valid,
    input [7:0] io_in_2_bits,
    output io_in_3_ready,
    input  io_in_3_valid,
    input [7:0] io_in_3_bits,
    input  io_out_ready,
    output io_out_valid,
    output[7:0] io_out_bits,
    output[1:0] io_chosen
);

  wire[1:0] T0;
  wire[1:0] T1;
  wire[1:0] T2;
  wire[1:0] T3;
  wire[1:0] T4;
  wire[1:0] T5;
  wire[1:0] T6;
  wire T7;
  wire T8;
  reg[1:0] R9;
  wire T10;
  wire T11;
  wire T12;
  wire T13;
  wire T14;
  wire[7:0] T15;
  wire[7:0] T16;
  wire T17;
  wire[1:0] T18;
  wire[7:0] T19;
  wire T20;
  wire T21;
  wire T22;
  wire T23;
  wire T24;
  wire T25;
  wire T26;
  wire T27;
  wire T28;
  wire T29;
  wire T30;
  wire T31;
  wire T32;
  wire T33;
  wire T34;
  wire T35;
  wire T36;
  wire T37;
  wire T38;
  wire T39;
  wire T40;
  wire T41;
  wire T42;
  wire T43;
  wire T44;
  wire T45;
  wire T46;
  wire T47;
  wire T48;
  wire T49;
  wire T50;
  wire T51;
  wire T52;
  wire T53;
  wire T54;
  wire T55;
  wire T56;
  wire T57;
  wire T58;
  wire T59;
  wire T60;
  wire T61;
  wire T62;
  wire T63;
  wire T64;
  wire T65;
  wire T66;
  wire T67;
  wire T68;
  wire T69;
  wire T70;
  wire T71;
  wire T72;
  wire T73;
  wire T74;
  wire T75;
  wire T76;
  wire T77;
  wire T78;
  wire T79;
  wire T80;
  wire T81;
  wire T82;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    R9 = {1{$random}};
  end
`endif

  assign io_chosen = T0;
  assign T0 = T1;
  assign T1 = T13 ? 2'h1 : T2;
  assign T2 = T11 ? 2'h2 : T3;
  assign T3 = T7 ? 2'h3 : T4;
  assign T4 = io_in_0_valid ? 2'h0 : T5;
  assign T5 = io_in_1_valid ? 2'h1 : T6;
  assign T6 = io_in_2_valid ? 2'h2 : 2'h3;
  assign T7 = io_in_3_valid && T8;
  assign T8 = R9 < 2'h3;
  assign T10 = io_out_ready && io_out_valid;
  assign T11 = io_in_2_valid && T12;
  assign T12 = R9 < 2'h2;
  assign T13 = io_in_1_valid && T14;
  assign T14 = R9 < 2'h1;
  assign io_out_bits = T15;
  assign T15 = T21 ? T19 : T16;
  assign T16 = T17 ? io_in_1_bits : io_in_0_bits;
  assign T17 = T18[1'h0:1'h0];
  assign T18 = T0;
  assign T19 = T20 ? io_in_3_bits : io_in_2_bits;
  assign T20 = T18[1'h0:1'h0];
  assign T21 = T18[1'h1:1'h1];
  assign io_out_valid = T22;
  assign T22 = T27 ? T25 : T23;
  assign T23 = T24 ? io_in_1_valid : io_in_0_valid;
  assign T24 = T18[1'h0:1'h0];
  assign T25 = T26 ? io_in_3_valid : io_in_2_valid;
  assign T26 = T18[1'h0:1'h0];
  assign T27 = T18[1'h1:1'h1];
  assign io_in_3_ready = T28;
  assign T28 = T29 && io_out_ready;
  assign T29 = T30;
  assign T30 = T46 || T31;
  assign T31 = ! T32;
  assign T32 = T33 || io_in_2_valid;
  assign T33 = T34 || io_in_1_valid;
  assign T34 = T35 || io_in_0_valid;
  assign T35 = T38 || T36;
  assign T36 = io_in_3_valid && T37;
  assign T37 = R9 < 2'h3;
  assign T38 = T41 || T39;
  assign T39 = io_in_2_valid && T40;
  assign T40 = R9 < 2'h2;
  assign T41 = T44 || T42;
  assign T42 = io_in_1_valid && T43;
  assign T43 = R9 < 2'h1;
  assign T44 = io_in_0_valid && T45;
  assign T45 = R9 < 2'h0;
  assign T46 = T48 && T47;
  assign T47 = R9 < 2'h3;
  assign T48 = ! T49;
  assign T49 = T50 || T39;
  assign T50 = T44 || T42;
  assign io_in_2_ready = T51;
  assign T51 = T52 && io_out_ready;
  assign T52 = T53;
  assign T53 = T60 || T54;
  assign T54 = ! T55;
  assign T55 = T56 || io_in_1_valid;
  assign T56 = T57 || io_in_0_valid;
  assign T57 = T58 || T36;
  assign T58 = T59 || T39;
  assign T59 = T44 || T42;
  assign T60 = T62 && T61;
  assign T61 = R9 < 2'h2;
  assign T62 = ! T63;
  assign T63 = T44 || T42;
  assign io_in_1_ready = T64;
  assign T64 = T65 && io_out_ready;
  assign T65 = T66;
  assign T66 = T72 || T67;
  assign T67 = ! T68;
  assign T68 = T69 || io_in_0_valid;
  assign T69 = T70 || T36;
  assign T70 = T71 || T39;
  assign T71 = T44 || T42;
  assign T72 = T74 && T73;
  assign T73 = R9 < 2'h1;
  assign T74 = ! T44;
  assign io_in_0_ready = T75;
  assign T75 = T76 && io_out_ready;
  assign T76 = T77;
  assign T77 = T82 || T78;
  assign T78 = ! T79;
  assign T79 = T80 || T36;
  assign T80 = T81 || T39;
  assign T81 = T44 || T42;
  assign T82 = R9 < 2'h0;

  always @(posedge clk) begin
    if(reset) begin
      R9 <= 2'h0;
    end else if(T10) begin
      R9 <= T0;
    end
  end
endmodule

