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

  wire[1:0] chosen;
  wire[1:0] choose;
  wire[1:0] T0;
  wire[1:0] T1;
  wire[1:0] T2;
  wire[1:0] T3;
  wire[1:0] T4;
  wire T5;
  wire T6;
  reg [1:0] last_grant;
  wire[1:0] T77;
  wire[1:0] T7;
  wire T8;
  wire T9;
  wire T10;
  wire T11;
  wire T12;
  wire[7:0] T13;
  wire[7:0] T14;
  wire T15;
  wire[1:0] T16;
  wire[7:0] T17;
  wire T18;
  wire T19;
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

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    last_grant = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_chosen = chosen;
  assign chosen = choose;
  assign choose = T11 ? 2'h1 : T0;
  assign T0 = T9 ? 2'h2 : T1;
  assign T1 = T5 ? 2'h3 : T2;
  assign T2 = io_in_0_valid ? 2'h0 : T3;
  assign T3 = io_in_1_valid ? 2'h1 : T4;
  assign T4 = io_in_2_valid ? 2'h2 : 2'h3;
  assign T5 = io_in_3_valid & T6;
  assign T6 = last_grant < 2'h3;
  assign T77 = reset ? 2'h0 : T7;
  assign T7 = T8 ? chosen : last_grant;
  assign T8 = io_out_ready & io_out_valid;
  assign T9 = io_in_2_valid & T10;
  assign T10 = last_grant < 2'h2;
  assign T11 = io_in_1_valid & T12;
  assign T12 = last_grant < 2'h1;
  assign io_out_bits = T13;
  assign T13 = T19 ? T17 : T14;
  assign T14 = T15 ? io_in_1_bits : io_in_0_bits;
  assign T15 = T16[1'h0];
  assign T16 = chosen;
  assign T17 = T18 ? io_in_3_bits : io_in_2_bits;
  assign T18 = T16[1'h0];
  assign T19 = T16[1'h1];
  assign io_out_valid = T20;
  assign T20 = T25 ? T23 : T21;
  assign T21 = T22 ? io_in_1_valid : io_in_0_valid;
  assign T22 = T16[1'h0];
  assign T23 = T24 ? io_in_3_valid : io_in_2_valid;
  assign T24 = T16[1'h0];
  assign T25 = T16[1'h1];
  assign io_in_0_ready = T26;
  assign T26 = T27 & io_out_ready;
  assign T27 = T40 | T28;
  assign T28 = T29 ^ 1'h1;
  assign T29 = T32 | T30;
  assign T30 = io_in_3_valid & T31;
  assign T31 = last_grant < 2'h3;
  assign T32 = T35 | T33;
  assign T33 = io_in_2_valid & T34;
  assign T34 = last_grant < 2'h2;
  assign T35 = T38 | T36;
  assign T36 = io_in_1_valid & T37;
  assign T37 = last_grant < 2'h1;
  assign T38 = io_in_0_valid & T39;
  assign T39 = last_grant < 2'h0;
  assign T40 = last_grant < 2'h0;
  assign io_in_1_ready = T41;
  assign T41 = T42 & io_out_ready;
  assign T42 = T48 | T43;
  assign T43 = T44 ^ 1'h1;
  assign T44 = T45 | io_in_0_valid;
  assign T45 = T46 | T30;
  assign T46 = T47 | T33;
  assign T47 = T38 | T36;
  assign T48 = T50 & T49;
  assign T49 = last_grant < 2'h1;
  assign T50 = T38 ^ 1'h1;
  assign io_in_2_ready = T51;
  assign T51 = T52 & io_out_ready;
  assign T52 = T59 | T53;
  assign T53 = T54 ^ 1'h1;
  assign T54 = T55 | io_in_1_valid;
  assign T55 = T56 | io_in_0_valid;
  assign T56 = T57 | T30;
  assign T57 = T58 | T33;
  assign T58 = T38 | T36;
  assign T59 = T61 & T60;
  assign T60 = last_grant < 2'h2;
  assign T61 = T62 ^ 1'h1;
  assign T62 = T38 | T36;
  assign io_in_3_ready = T63;
  assign T63 = T64 & io_out_ready;
  assign T64 = T72 | T65;
  assign T65 = T66 ^ 1'h1;
  assign T66 = T67 | io_in_2_valid;
  assign T67 = T68 | io_in_1_valid;
  assign T68 = T69 | io_in_0_valid;
  assign T69 = T70 | T30;
  assign T70 = T71 | T33;
  assign T71 = T38 | T36;
  assign T72 = T74 & T73;
  assign T73 = last_grant < 2'h3;
  assign T74 = T75 ^ 1'h1;
  assign T75 = T76 | T33;
  assign T76 = T38 | T36;

  always @(posedge clk) begin
    if(reset) begin
      last_grant <= 2'h0;
    end else if(T8) begin
      last_grant <= chosen;
    end
  end
endmodule

