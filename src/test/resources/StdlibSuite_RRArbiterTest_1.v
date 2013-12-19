module StdlibSuite_RRArbiterTest_1(input clk, input reset,
    output io_in_0_ready,
    input io_in_0_valid,
    input [7:0] io_in_0_bits,
    output io_in_1_ready,
    input io_in_1_valid,
    input [7:0] io_in_1_bits,
    output io_in_2_ready,
    input io_in_2_valid,
    input [7:0] io_in_2_bits,
    output io_in_3_ready,
    input io_in_3_valid,
    input [7:0] io_in_3_bits,
    input io_out_ready,
    output io_out_valid,
    output [7:0] io_out_bits,
    output [1:0] io_chosen
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;
  reg [1:0] R4;
  wire [1:0] T5;
  wire T6;
  wire T7;
  wire T8;
  wire [1:0] T9;
  wire [1:0] T10;
  wire T11;
  wire T12;
  wire [1:0] T13;
  wire T14;
  wire T15;
  wire [1:0] T16;
  wire T17;
  wire T18;
  wire [1:0] T19;
  wire [1:0] T20;
  wire [1:0] T21;
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
  wire [7:0] T77;
  wire T78;
  wire [7:0] T79;
  wire T80;
  wire [7:0] T81;
  wire T82;

  assign io_in_0_ready = T0;
  assign T0 = T1 && io_out_ready;
  assign T1 = T2;
  assign T2 = T3 || T26;
  assign T3 = 2'h0/* 0*/ > R4;
  assign T5 = T6 ? T9 : R4;
  assign T6 = io_out_ready && io_out_valid;
  assign io_out_valid = T7;
  assign T7 = T8 ? T22 : T24;
  assign T8 = T9[1'h1/* 1*/:1'h1/* 1*/];
  assign T9 = T10;
  assign T10 = T11 ? 1'h1/* 1*/ : T13;
  assign T11 = io_in_1_valid && T12;
  assign T12 = 2'h1/* 1*/ > R4;
  assign T13 = T14 ? 2'h2/* 2*/ : T16;
  assign T14 = io_in_2_valid && T15;
  assign T15 = 2'h2/* 2*/ > R4;
  assign T16 = T17 ? 2'h3/* 3*/ : T19;
  assign T17 = io_in_3_valid && T18;
  assign T18 = 2'h3/* 3*/ > R4;
  assign T19 = io_in_0_valid ? 1'h0/* 0*/ : T20;
  assign T20 = io_in_1_valid ? 1'h1/* 1*/ : T21;
  assign T21 = io_in_2_valid ? 2'h2/* 2*/ : 2'h3/* 3*/;
  assign T22 = T23 ? io_in_3_valid : io_in_2_valid;
  assign T23 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T24 = T25 ? io_in_1_valid : io_in_0_valid;
  assign T25 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T26 = ! T27;
  assign T27 = T28 || T36;
  assign T28 = T29 || T34;
  assign T29 = T30 || T32;
  assign T30 = io_in_0_valid && T31;
  assign T31 = 2'h0/* 0*/ > R4;
  assign T32 = io_in_1_valid && T33;
  assign T33 = 2'h1/* 1*/ > R4;
  assign T34 = io_in_2_valid && T35;
  assign T35 = 2'h2/* 2*/ > R4;
  assign T36 = io_in_3_valid && T37;
  assign T37 = 2'h3/* 3*/ > R4;
  assign io_in_1_ready = T38;
  assign T38 = T39 && io_out_ready;
  assign T39 = T40;
  assign T40 = T41 || T44;
  assign T41 = T42 && T43;
  assign T42 = ! T30;
  assign T43 = 2'h1/* 1*/ > R4;
  assign T44 = ! T45;
  assign T45 = T46 || io_in_0_valid;
  assign T46 = T47 || T36;
  assign T47 = T48 || T34;
  assign T48 = T30 || T32;
  assign io_in_2_ready = T49;
  assign T49 = T50 && io_out_ready;
  assign T50 = T51;
  assign T51 = T52 || T56;
  assign T52 = T53 && T55;
  assign T53 = ! T54;
  assign T54 = T30 || T32;
  assign T55 = 2'h2/* 2*/ > R4;
  assign T56 = ! T57;
  assign T57 = T58 || io_in_1_valid;
  assign T58 = T59 || io_in_0_valid;
  assign T59 = T60 || T36;
  assign T60 = T61 || T34;
  assign T61 = T30 || T32;
  assign io_in_3_ready = T62;
  assign T62 = T63 && io_out_ready;
  assign T63 = T64;
  assign T64 = T65 || T70;
  assign T65 = T66 && T69;
  assign T66 = ! T67;
  assign T67 = T68 || T34;
  assign T68 = T30 || T32;
  assign T69 = 2'h3/* 3*/ > R4;
  assign T70 = ! T71;
  assign T71 = T72 || io_in_2_valid;
  assign T72 = T73 || io_in_1_valid;
  assign T73 = T74 || io_in_0_valid;
  assign T74 = T75 || T36;
  assign T75 = T76 || T34;
  assign T76 = T30 || T32;
  assign io_out_bits = T77;
  assign T77 = T78 ? T79 : T81;
  assign T78 = T9[1'h1/* 1*/:1'h1/* 1*/];
  assign T79 = T80 ? io_in_3_bits : io_in_2_bits;
  assign T80 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T81 = T82 ? io_in_1_bits : io_in_0_bits;
  assign T82 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign io_chosen = T9;

  always @(posedge clk) begin
    if(reset) begin
      R4 <= 2'h0/* 0*/;
    end else if(T6) begin
      R4 <= T5;
    end
  end
endmodule

