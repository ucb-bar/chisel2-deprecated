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
  wire[1:0] T10;
  wire[1:0] T11;
  wire T12;
  wire T13;
  wire T14;
  wire T15;
  wire T16;
  wire[7:0] T17;
  wire[7:0] T18;
  wire T19;
  wire[1:0] T20;
  wire[7:0] T21;
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
  wire T83;
  wire T84;

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    R9 = {1{$random}};
  end
`endif

  assign io_chosen = T0;
  assign T0 = T1;
  assign T1 = T15 ? 2'h1 : T2;
  assign T2 = T13 ? 2'h2 : T3;
  assign T3 = T7 ? 2'h3 : T4;
  assign T4 = io_in_0_valid ? 2'h0 : T5;
  assign T5 = io_in_1_valid ? 2'h1 : T6;
  assign T6 = io_in_2_valid ? 2'h2 : 2'h3;
  assign T7 = io_in_3_valid & T8;
  assign T8 = R9 < 2'h3;
  assign T10 = reset ? 2'h0 : T11;
  assign T11 = T12 ? T0 : R9;
  assign T12 = io_out_ready & io_out_valid;
  assign T13 = io_in_2_valid & T14;
  assign T14 = R9 < 2'h2;
  assign T15 = io_in_1_valid & T16;
  assign T16 = R9 < 2'h1;
  assign io_out_bits = T17;
  assign T17 = T23 ? T21 : T18;
  assign T18 = T19 ? io_in_1_bits : io_in_0_bits;
  assign T19 = T20[1'h0:1'h0];
  assign T20 = T0;
  assign T21 = T22 ? io_in_3_bits : io_in_2_bits;
  assign T22 = T20[1'h0:1'h0];
  assign T23 = T20[1'h1:1'h1];
  assign io_out_valid = T24;
  assign T24 = T29 ? T27 : T25;
  assign T25 = T26 ? io_in_1_valid : io_in_0_valid;
  assign T26 = T20[1'h0:1'h0];
  assign T27 = T28 ? io_in_3_valid : io_in_2_valid;
  assign T28 = T20[1'h0:1'h0];
  assign T29 = T20[1'h1:1'h1];
  assign io_in_3_ready = T30;
  assign T30 = T31 & io_out_ready;
  assign T31 = T32;
  assign T32 = T48 | T33;
  assign T33 = T34 == 1'h0;
  assign T34 = T35 | io_in_2_valid;
  assign T35 = T36 | io_in_1_valid;
  assign T36 = T37 | io_in_0_valid;
  assign T37 = T40 | T38;
  assign T38 = io_in_3_valid & T39;
  assign T39 = R9 < 2'h3;
  assign T40 = T43 | T41;
  assign T41 = io_in_2_valid & T42;
  assign T42 = R9 < 2'h2;
  assign T43 = T46 | T44;
  assign T44 = io_in_1_valid & T45;
  assign T45 = R9 < 2'h1;
  assign T46 = io_in_0_valid & T47;
  assign T47 = R9 < 2'h0;
  assign T48 = T50 & T49;
  assign T49 = R9 < 2'h3;
  assign T50 = T51 == 1'h0;
  assign T51 = T52 | T41;
  assign T52 = T46 | T44;
  assign io_in_2_ready = T53;
  assign T53 = T54 & io_out_ready;
  assign T54 = T55;
  assign T55 = T62 | T56;
  assign T56 = T57 == 1'h0;
  assign T57 = T58 | io_in_1_valid;
  assign T58 = T59 | io_in_0_valid;
  assign T59 = T60 | T38;
  assign T60 = T61 | T41;
  assign T61 = T46 | T44;
  assign T62 = T64 & T63;
  assign T63 = R9 < 2'h2;
  assign T64 = T65 == 1'h0;
  assign T65 = T46 | T44;
  assign io_in_1_ready = T66;
  assign T66 = T67 & io_out_ready;
  assign T67 = T68;
  assign T68 = T74 | T69;
  assign T69 = T70 == 1'h0;
  assign T70 = T71 | io_in_0_valid;
  assign T71 = T72 | T38;
  assign T72 = T73 | T41;
  assign T73 = T46 | T44;
  assign T74 = T76 & T75;
  assign T75 = R9 < 2'h1;
  assign T76 = T46 == 1'h0;
  assign io_in_0_ready = T77;
  assign T77 = T78 & io_out_ready;
  assign T78 = T79;
  assign T79 = T84 | T80;
  assign T80 = T81 == 1'h0;
  assign T81 = T82 | T38;
  assign T82 = T83 | T41;
  assign T83 = T46 | T44;
  assign T84 = R9 < 2'h0;

  always @(posedge clk) begin
    if(reset) begin
      R9 <= 2'h0;
    end else if(T12) begin
      R9 <= T0;
    end
  end
endmodule

