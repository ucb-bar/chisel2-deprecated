#include "VerifSuite_CppAssertComp_1.h"

void VerifSuite_CppAssertComp_1_t::init ( bool rand_init ) {
}
void VerifSuite_CppAssertComp_1_t::clock_lo_clk ( dat_t<1> reset ) {
  val_t T1__w0;
  { T1__w0 = VerifSuite_CppAssertComp_1__io_y.values[0] | VerifSuite_CppAssertComp_1__io_x.values[0] << 8; }
  { VerifSuite_CppAssertComp_1__io_z.values[0] = T1__w0; }
  ASSERT(reset.values[0], "failure");
}
void VerifSuite_CppAssertComp_1_t::clock_hi_clk ( dat_t<1> reset ) {
}
int VerifSuite_CppAssertComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_lo_clk( reset );
  if (clk_cnt == 0) clock_hi_clk( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}
void VerifSuite_CppAssertComp_1_t::print ( FILE* f ) {
}
bool VerifSuite_CppAssertComp_1_t::scan ( FILE* f ) {
  return(!feof(f));
}
void VerifSuite_CppAssertComp_1_t::dump(FILE *f, int t) {
}
