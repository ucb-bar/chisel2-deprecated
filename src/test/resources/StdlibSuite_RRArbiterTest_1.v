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
  wire[1:0] T80;
  wire[1:0] T10;
  wire T11;
  wire T19;
  wire T20;
  wire T21;
  wire T22;
  wire[7:0] T23;
  wire[7:0] T24;
  wire T25;
  wire[1:0] T15;
  wire[7:0] T26;
  wire T27;
  wire T28;
  wire T12;
  wire T13;
  wire T14;
  wire T16;
  wire T17;
  wire T18;
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

`ifndef SYNTHESIS
  integer initvar;
  initial begin
    #0.002;
    R9 = {1{$random}};
  end
`endif

  assign io_chosen = T0;
  assign T0 = T1;
  assign T1 = T21 ? 2'h1 : T2;
  assign T2 = T19 ? 2'h2 : T3;
  assign T3 = T7 ? 2'h3 : T4;
  assign T4 = io_in_0_valid ? 2'h0 : T5;
  assign T5 = io_in_1_valid ? 2'h1 : T6;
  assign T6 = io_in_2_valid ? 2'h2 : 2'h3;
  assign T7 = io_in_3_valid & T8;
  assign T8 = R9 < 2'h3;
  assign T80 = reset ? 2'h0 : T10;
  assign T10 = T11 ? T0 : R9;
  assign T11 = io_out_ready & io_out_valid;
  assign T19 = io_in_2_valid & T20;
  assign T20 = R9 < 2'h2;
  assign T21 = io_in_1_valid & T22;
  assign T22 = R9 < 2'h1;
  assign io_out_bits = T23;
  assign T23 = T28 ? T26 : T24;
  assign T24 = T25 ? io_in_1_bits : io_in_0_bits;
  assign T25 = T15[1'h0:1'h0];
  assign T15 = T0;
  assign T26 = T27 ? io_in_3_bits : io_in_2_bits;
  assign T27 = T15[1'h0:1'h0];
  assign T28 = T15[1'h1:1'h1];
  assign io_out_valid = T12;
  assign T12 = T18 ? T16 : T13;
  assign T13 = T14 ? io_in_1_valid : io_in_0_valid;
  assign T14 = T15[1'h0:1'h0];
  assign T16 = T17 ? io_in_3_valid : io_in_2_valid;
  assign T17 = T15[1'h0:1'h0];
  assign T18 = T15[1'h1:1'h1];
  assign io_in_0_ready = T29;
  assign T29 = T30 & io_out_ready;
  assign T30 = T43 | T31;
  assign T31 = T32 ^ 1'h1;
  assign T32 = T35 | T33;
  assign T33 = io_in_3_valid & T34;
  assign T34 = R9 < 2'h3;
  assign T35 = T38 | T36;
  assign T36 = io_in_2_valid & T37;
  assign T37 = R9 < 2'h2;
  assign T38 = T41 | T39;
  assign T39 = io_in_1_valid & T40;
  assign T40 = R9 < 2'h1;
  assign T41 = io_in_0_valid & T42;
  assign T42 = R9 < 2'h0;
  assign T43 = R9 < 2'h0;
  assign io_in_1_ready = T44;
  assign T44 = T45 & io_out_ready;
  assign T45 = T51 | T46;
  assign T46 = T47 ^ 1'h1;
  assign T47 = T48 | io_in_0_valid;
  assign T48 = T49 | T33;
  assign T49 = T50 | T36;
  assign T50 = T41 | T39;
  assign T51 = T53 & T52;
  assign T52 = R9 < 2'h1;
  assign T53 = T41 ^ 1'h1;
  assign io_in_2_ready = T54;
  assign T54 = T55 & io_out_ready;
  assign T55 = T62 | T56;
  assign T56 = T57 ^ 1'h1;
  assign T57 = T58 | io_in_1_valid;
  assign T58 = T59 | io_in_0_valid;
  assign T59 = T60 | T33;
  assign T60 = T61 | T36;
  assign T61 = T41 | T39;
  assign T62 = T64 & T63;
  assign T63 = R9 < 2'h2;
  assign T64 = T65 ^ 1'h1;
  assign T65 = T41 | T39;
  assign io_in_3_ready = T66;
  assign T66 = T67 & io_out_ready;
  assign T67 = T75 | T68;
  assign T68 = T69 ^ 1'h1;
  assign T69 = T70 | io_in_2_valid;
  assign T70 = T71 | io_in_1_valid;
  assign T71 = T72 | io_in_0_valid;
  assign T72 = T73 | T33;
  assign T73 = T74 | T36;
  assign T74 = T41 | T39;
  assign T75 = T77 & T76;
  assign T76 = R9 < 2'h3;
  assign T77 = T78 ^ 1'h1;
  assign T78 = T79 | T36;
  assign T79 = T41 | T39;

  always @(posedge clk) begin
    if(reset) begin
      R9 <= 2'h0;
    end else if(T11) begin
      R9 <= T0;
    end
  end
endmodule

