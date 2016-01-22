#include "DelaySuite_SeqReadBundle_1.h"

void DelaySuite_SeqReadBundle_1_t::init ( val_t rand_init ) {
  this->__srand(rand_init);
  DelaySuite_SeqReadBundle_1__R9.randomize(&__rand_seed);
  DelaySuite_SeqReadBundle_1__mem.randomize(&__rand_seed);
  clk.len = 1;
  clk.cnt = 0;
  clk.values[0] = 0;
}
int DelaySuite_SeqReadBundle_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk.cnt < min) min = clk.cnt;
  clk.cnt-=min;
  if (clk.cnt == 0) clock_lo( reset );
  if (clk.cnt == 0) clock_hi( reset );
  if (clk.cnt == 0) clk.cnt = clk.len;
  return min;
}
void DelaySuite_SeqReadBundle_1_t::print ( FILE* f ) {
}
void DelaySuite_SeqReadBundle_1_t::print ( std::ostream& s ) {
}
void DelaySuite_SeqReadBundle_1_t::dump_init ( FILE* f ) {
}
void DelaySuite_SeqReadBundle_1_t::dump ( FILE* f, int t, dat_t<1> reset ) {
}
void DelaySuite_SeqReadBundle_1_t::clock_lo ( dat_t<1> reset, bool assert_fire ) {
  val_t T0[2];
  { T0[0] = DelaySuite_SeqReadBundle_1__mem.get(DelaySuite_SeqReadBundle_1__R9.values[0], 0); T0[1] = DelaySuite_SeqReadBundle_1__mem.get(DelaySuite_SeqReadBundle_1__R9.values[0], 1);}
  val_t T1;
  { T1 = T0[1] >> 40;}
  T1 = T1 & 0xffL;
  { DelaySuite_SeqReadBundle_1__io_out_1_a_a.values[0] = T1;}
  val_t T2;
  { T2 = T0[1] >> 24;}
  T2 = T2 & 0xffffL;
  { DelaySuite_SeqReadBundle_1__io_out_1_a_b.values[0] = T2;}
  val_t T3;
  { T3 = T0[0] >> 56 | T0[1] << 8;}
  T3 = T3 & 0xffffffffL;
  { DelaySuite_SeqReadBundle_1__io_out_1_a_b_.values[0] = T3;}
  val_t T4;
  { T4 = T0[0] >> 48 | T0[1] << 16;}
  T4 = T4 & 0xffL;
  { DelaySuite_SeqReadBundle_1__io_out_0_a_a.values[0] = T4;}
  val_t T5;
  { T5 = T0[0] >> 32 | T0[1] << 32;}
  T5 = T5 & 0xffffL;
  { DelaySuite_SeqReadBundle_1__io_out_0_a_b.values[0] = T5;}
  { T6.values[0] = TERNARY_1(DelaySuite_SeqReadBundle_1__io_ren.values[0], DelaySuite_SeqReadBundle_1__io_raddr.values[0], DelaySuite_SeqReadBundle_1__R9.values[0]);}
  val_t T7;
  { T7 = DelaySuite_SeqReadBundle_1__io_in_0_a_b_.values[0] | DelaySuite_SeqReadBundle_1__io_in_0_a_b.values[0] << 32;}
  val_t T8;
  { T8 = T7 | DelaySuite_SeqReadBundle_1__io_in_0_a_a.values[0] << 48;}
  val_t T9;
  { T9 = DelaySuite_SeqReadBundle_1__io_in_1_a_b_.values[0] | DelaySuite_SeqReadBundle_1__io_in_1_a_b.values[0] << 32;}
  val_t T10;
  { T10 = T9 | DelaySuite_SeqReadBundle_1__io_in_1_a_a.values[0] << 48;}
  val_t T11[2];
  { T11[0] = T8 | T10 << 56; T11[1] = T10 >> 8;}
  { T12.values[0] = T11[0]; T12.values[1] = T11[1];}
  val_t T13;
  { T13 = T0[0];}
  T13 = T13 & 0xffffffffL;
  { DelaySuite_SeqReadBundle_1__io_out_0_a_b_.values[0] = T13;}
}
void DelaySuite_SeqReadBundle_1_t::clock_hi ( dat_t<1> reset ) {
  dat_t<4> DelaySuite_SeqReadBundle_1__R9__shadow = T6;
  { if (DelaySuite_SeqReadBundle_1__io_wen.values[0]) DelaySuite_SeqReadBundle_1__mem.put(DelaySuite_SeqReadBundle_1__io_waddr.values[0], 0, T12.values[0]); if (DelaySuite_SeqReadBundle_1__io_wen.values[0]) DelaySuite_SeqReadBundle_1__mem.put(DelaySuite_SeqReadBundle_1__io_waddr.values[0], 1, T12.values[1]);}
  DelaySuite_SeqReadBundle_1__R9 = T6;
}
