module StdlibSuite_RRArbiterTest_1(input clk, input reset,
    output io_in_3_ready,
    input  io_in_3_valid,
    input [7:0] io_in_3_bits,
    output io_in_2_ready,
    input  io_in_2_valid,
    input [7:0] io_in_2_bits,
    output io_in_1_ready,
    input  io_in_1_valid,
    input [7:0] io_in_1_bits,
    output io_in_0_ready,
    input  io_in_0_valid,
    input [7:0] io_in_0_bits,
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
  reg [1:0] R9;
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
  assign io_in_0_ready = T30;
  assign T30 = T31 & io_out_ready;
  assign T31 = T44 | T32;
  assign T32 = T33 ^ 1'h1;
  assign T33 = T36 | T34;
  assign T34 = io_in_3_valid & T35;
  assign T35 = R9 < 2'h3;
  assign T36 = T39 | T37;
  assign T37 = io_in_2_valid & T38;
  assign T38 = R9 < 2'h2;
  assign T39 = T42 | T40;
  assign T40 = io_in_1_valid & T41;
  assign T41 = R9 < 2'h1;
  assign T42 = io_in_0_valid & T43;
  assign T43 = R9 < 2'h0;
  assign T44 = R9 < 2'h0;
  assign io_in_1_ready = T45;
  assign T45 = T46 & io_out_ready;
  assign T46 = T52 | T47;
  assign T47 = T48 ^ 1'h1;
  assign T48 = T49 | io_in_0_valid;
  assign T49 = T50 | T34;
  assign T50 = T51 | T37;
  assign T51 = T42 | T40;
  assign T52 = T54 & T53;
  assign T53 = R9 < 2'h1;
  assign T54 = T42 ^ 1'h1;
  assign io_in_2_ready = T55;
  assign T55 = T56 & io_out_ready;
  assign T56 = T63 | T57;
  assign T57 = T58 ^ 1'h1;
  assign T58 = T59 | io_in_1_valid;
  assign T59 = T60 | io_in_0_valid;
  assign T60 = T61 | T34;
  assign T61 = T62 | T37;
  assign T62 = T42 | T40;
  assign T63 = T65 & T64;
  assign T64 = R9 < 2'h2;
  assign T65 = T66 ^ 1'h1;
  assign T66 = T42 | T40;
  assign io_in_3_ready = T67;
  assign T67 = T68 & io_out_ready;
  assign T68 = T76 | T69;
  assign T69 = T70 ^ 1'h1;
  assign T70 = T71 | io_in_2_valid;
  assign T71 = T72 | io_in_1_valid;
  assign T72 = T73 | io_in_0_valid;
  assign T73 = T74 | T34;
  assign T74 = T75 | T37;
  assign T75 = T42 | T40;
  assign T76 = T78 & T77;
  assign T77 = R9 < 2'h3;
  assign T78 = T79 ^ 1'h1;
  assign T79 = T80 | T37;
  assign T80 = T42 | T40;

  always @(posedge clk) begin
    if(reset) begin
      R9 <= 2'h0;
    end else if(T12) begin
      R9 <= T0;
    end
  end
endmodule

