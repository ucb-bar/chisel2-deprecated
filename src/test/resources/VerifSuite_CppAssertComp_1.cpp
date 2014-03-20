#include "VerifSuite_CppAssertComp_1.h"

void VerifSuite_CppAssertComp_1_t::init ( bool rand_init ) {
}
int VerifSuite_CppAssertComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_lo( reset );
  if (clk_cnt == 0) clock_hi( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}
void VerifSuite_CppAssertComp_1_t::print ( FILE* f ) {
}
void VerifSuite_CppAssertComp_1_t::dump(FILE *f, int t) {
}
void VerifSuite_CppAssertComp_1_t::clock_lo ( dat_t<1> reset ) {
  val_t T1__w0;
  { T1__w0 = VerifSuite_CppAssertComp_1__io_y.values[0] | VerifSuite_CppAssertComp_1__io_x.values[0] << 8; }
  { VerifSuite_CppAssertComp_1__io_z.values[0] = T1__w0; }
  ASSERT(reset.values[0], "failure");
}
void VerifSuite_CppAssertComp_1_t::clock_hi ( dat_t<1> reset ) {
}
void VerifSuite_CppAssertComp_1_api_t::init_mapping_table() {
  dat_table.clear();
  mem_table.clear();
  VerifSuite_CppAssertComp_1_t* mod_typed = dynamic_cast<VerifSuite_CppAssertComp_1_t*>(module);
  assert(mod_typed);
  dat_table["VerifSuite_CppAssertComp_1.io_y"] = new dat_api<8>(&mod_typed->VerifSuite_CppAssertComp_1__io_y, "VerifSuite_CppAssertComp_1.io_y", "");
  dat_table["VerifSuite_CppAssertComp_1.io_x"] = new dat_api<8>(&mod_typed->VerifSuite_CppAssertComp_1__io_x, "VerifSuite_CppAssertComp_1.io_x", "");
  dat_table["VerifSuite_CppAssertComp_1.io_z"] = new dat_api<16>(&mod_typed->VerifSuite_CppAssertComp_1__io_z, "VerifSuite_CppAssertComp_1.io_z", "");
}
