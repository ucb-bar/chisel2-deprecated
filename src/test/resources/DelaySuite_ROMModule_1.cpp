#include "DelaySuite_ROMModule_1.h"

void DelaySuite_ROMModule_1_t::init ( bool rand_init ) {
  { T0.put(0, 0, 0x1L); }
  { T0.put(1, 0, 0x2L); }
  { T0.put(2, 0, 0x3L); }
  nodes.clear();
  mems.clear();
  nodes["DelaySuite_ROMModule_1.io_addr"] = &DelaySuite_ROMModule_1__io_addr;
  nodes["DelaySuite_ROMModule_1.io_out"] = &DelaySuite_ROMModule_1__io_out;
}
int DelaySuite_ROMModule_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_lo( reset );
  if (clk_cnt == 0) clock_hi( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}
void DelaySuite_ROMModule_1_t::print ( FILE* f ) {
}
void DelaySuite_ROMModule_1_t::dump(FILE *f, int t) {
}
void DelaySuite_ROMModule_1_t::clock_lo ( dat_t<1> reset ) {
  val_t T1__w0;
  { T1__w0 = T0.get(DelaySuite_ROMModule_1__io_addr.values[0], 0); }
  { DelaySuite_ROMModule_1__io_out.values[0] = T1__w0; }
}
void DelaySuite_ROMModule_1_t::clock_hi ( dat_t<1> reset ) {
}
