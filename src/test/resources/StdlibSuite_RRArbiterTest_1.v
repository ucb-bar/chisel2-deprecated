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
  wire[1:0] T7;
  wire[1:0] T8;
  wire T9;
  wire T10;
  reg[1:0] R11;
  wire T12;
  wire T13;
  wire T14;
  wire[1:0] T15;
  wire T16;
  wire T17;
  wire[1:0] T18;
  wire[7:0] T19;
  wire[7:0] T20;
  wire T21;
  wire[1:0] T22;
  wire[7:0] T23;
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
  wire[1:0] T48;
  wire T49;
  wire T50;
  wire[1:0] T51;
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
  wire[1:0] T80;
  wire T81;
  wire T82;
  wire T83;
  wire T84;
  wire T85;
  wire T86;
  wire T87;
  wire T88;
  wire T89;
  wire[1:0] T90;

  assign io_chosen = T0;
  assign T0 = T1;
  assign T1 = T16 ? T15 : T2;
  assign T2 = T13 ? 2'h2/* 2*/ : T3;
  assign T3 = T9 ? 2'h3/* 3*/ : T4;
  assign T4 = io_in_0_valid ? T8 : T5;
  assign T5 = io_in_1_valid ? T7 : T6;
  assign T6 = io_in_2_valid ? 2'h2/* 2*/ : 2'h3/* 3*/;
  assign T7 = {1'h0/* 0*/, 1'h1/* 1*/};
  assign T8 = {1'h0/* 0*/, 1'h0/* 0*/};
  assign T9 = io_in_3_valid && T10;
  assign T10 = R11 < 2'h3/* 3*/;
  assign T12 = io_out_ready && io_out_valid;
  assign T13 = io_in_2_valid && T14;
  assign T14 = R11 < 2'h2/* 2*/;
  assign T15 = {1'h0/* 0*/, 1'h1/* 1*/};
  assign T16 = io_in_1_valid && T17;
  assign T17 = R11 < T18;
  assign T18 = {1'h0/* 0*/, 1'h1/* 1*/};
  assign io_out_bits = T19;
  assign T19 = T25 ? T23 : T20;
  assign T20 = T21 ? io_in_1_bits : io_in_0_bits;
  assign T21 = T22[1'h0/* 0*/:1'h0/* 0*/];
  assign T22 = T0;
  assign T23 = T24 ? io_in_3_bits : io_in_2_bits;
  assign T24 = T22[1'h0/* 0*/:1'h0/* 0*/];
  assign T25 = T22[1'h1/* 1*/:1'h1/* 1*/];
  assign io_out_valid = T26;
  assign T26 = T31 ? T29 : T27;
  assign T27 = T28 ? io_in_1_valid : io_in_0_valid;
  assign T28 = T22[1'h0/* 0*/:1'h0/* 0*/];
  assign T29 = T30 ? io_in_3_valid : io_in_2_valid;
  assign T30 = T22[1'h0/* 0*/:1'h0/* 0*/];
  assign T31 = T22[1'h1/* 1*/:1'h1/* 1*/];
  assign io_in_3_ready = T32;
  assign T32 = T33 && io_out_ready;
  assign T33 = T34;
  assign T34 = T52 || T35;
  assign T35 = ! T36;
  assign T36 = T37 || io_in_2_valid;
  assign T37 = T38 || io_in_1_valid;
  assign T38 = T39 || io_in_0_valid;
  assign T39 = T42 || T40;
  assign T40 = io_in_3_valid && T41;
  assign T41 = R11 < 2'h3/* 3*/;
  assign T42 = T45 || T43;
  assign T43 = io_in_2_valid && T44;
  assign T44 = R11 < 2'h2/* 2*/;
  assign T45 = T49 || T46;
  assign T46 = io_in_1_valid && T47;
  assign T47 = R11 < T48;
  assign T48 = {1'h0/* 0*/, 1'h1/* 1*/};
  assign T49 = io_in_0_valid && T50;
  assign T50 = R11 < T51;
  assign T51 = {1'h0/* 0*/, 1'h0/* 0*/};
  assign T52 = T54 && T53;
  assign T53 = R11 < 2'h3/* 3*/;
  assign T54 = ! T55;
  assign T55 = T56 || T43;
  assign T56 = T49 || T46;
  assign io_in_2_ready = T57;
  assign T57 = T58 && io_out_ready;
  assign T58 = T59;
  assign T59 = T66 || T60;
  assign T60 = ! T61;
  assign T61 = T62 || io_in_1_valid;
  assign T62 = T63 || io_in_0_valid;
  assign T63 = T64 || T40;
  assign T64 = T65 || T43;
  assign T65 = T49 || T46;
  assign T66 = T68 && T67;
  assign T67 = R11 < 2'h2/* 2*/;
  assign T68 = ! T69;
  assign T69 = T49 || T46;
  assign io_in_1_ready = T70;
  assign T70 = T71 && io_out_ready;
  assign T71 = T72;
  assign T72 = T78 || T73;
  assign T73 = ! T74;
  assign T74 = T75 || io_in_0_valid;
  assign T75 = T76 || T40;
  assign T76 = T77 || T43;
  assign T77 = T49 || T46;
  assign T78 = T81 && T79;
  assign T79 = R11 < T80;
  assign T80 = {1'h0/* 0*/, 1'h1/* 1*/};
  assign T81 = ! T49;
  assign io_in_0_ready = T82;
  assign T82 = T83 && io_out_ready;
  assign T83 = T84;
  assign T84 = T89 || T85;
  assign T85 = ! T86;
  assign T86 = T87 || T40;
  assign T87 = T88 || T43;
  assign T88 = T49 || T46;
  assign T89 = R11 < T90;
  assign T90 = {1'h0/* 0*/, 1'h0/* 0*/};

  always @(posedge clk) begin
    if(reset) begin
      R11 <= 2'h0/* 0*/;
    end else if(T12) begin
      R11 <= T0;
    end
  end
endmodule

