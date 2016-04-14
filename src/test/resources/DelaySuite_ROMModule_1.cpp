#include "DelaySuite_ROMModule_1.h"

void DelaySuite_ROMModule_1_t::init ( val_t rand_init ) {
  this->__srand(rand_init);
  T1.randomize(&__rand_seed);
  { T1.put(0, 0, 0x1L);}
  { T1.put(1, 0, 0x2L);}
  { T1.put(2, 0, 0x3L);}
  clk.len = 1;
  clk.cnt = 0;
  clk.values[0] = 0;
}
int DelaySuite_ROMModule_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk.cnt < min) min = clk.cnt;
  clk.cnt-=min;
  if (clk.cnt == 0) clock_lo( reset );
  if (!reset.to_bool()) print( std::cerr );
  if (clk.cnt == 0) clock_hi( reset );
  if (clk.cnt == 0) clk.cnt = clk.len;
  return min;
}
void DelaySuite_ROMModule_1_t::print ( FILE* f ) {
}
void DelaySuite_ROMModule_1_t::print ( std::ostream& s ) {
}
void DelaySuite_ROMModule_1_t::dump_init ( FILE* f ) {
}
void DelaySuite_ROMModule_1_t::dump ( FILE* f, val_t t, dat_t<1> reset ) {
}
void DelaySuite_ROMModule_1_t::clock_lo ( dat_t<1> reset, bool assert_fire ) {
  val_t T0;
  { T0 = T1.get(DelaySuite_ROMModule_1__io_addr.values[0], 0);}
  { DelaySuite_ROMModule_1__io_out.values[0] = T0;}
}
void DelaySuite_ROMModule_1_t::clock_hi ( dat_t<1> reset ) {
}
