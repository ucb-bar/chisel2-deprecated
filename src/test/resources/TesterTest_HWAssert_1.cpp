#include "TesterTest_HWAssert_1.h"

void TesterTest_HWAssert_1_t::init ( val_t rand_init ) {
  this->__srand(rand_init);
  TesterTest_HWAssert_1__reg.randomize(&__rand_seed);
}
int TesterTest_HWAssert_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_hi( reset );
  if (clk_cnt == 0) clock_lo( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}
mod_t* TesterTest_HWAssert_1_t::clone() {
  mod_t* cloned = new TesterTest_HWAssert_1_t(*this);
  return cloned;
}
bool TesterTest_HWAssert_1_t::set_circuit_from ( mod_t* src ) {
  TesterTest_HWAssert_1_t* mod_typed = dynamic_cast<TesterTest_HWAssert_1_t*>(src);
  assert(mod_typed);
  TesterTest_HWAssert_1__io_out = mod_typed->TesterTest_HWAssert_1__io_out;
  TesterTest_HWAssert_1__io_in = mod_typed->TesterTest_HWAssert_1__io_in;
  T0 = mod_typed->T0;
  TesterTest_HWAssert_1__reg = mod_typed->TesterTest_HWAssert_1__reg;
  T3 = mod_typed->T3;
  clk = mod_typed->clk;
  clk_cnt = mod_typed->clk_cnt;
  return true;
}
void TesterTest_HWAssert_1_t::print ( FILE* f ) {
}
void TesterTest_HWAssert_1_t::print ( std::ostream& s ) {
}
void TesterTest_HWAssert_1_t::dump_init ( FILE* f ) {
}
void TesterTest_HWAssert_1_t::dump ( FILE* f, int t ) {
}
void TesterTest_HWAssert_1_t::clock_lo ( dat_t<1> reset ) {
  { TesterTest_HWAssert_1__io_out.values[0] = TesterTest_HWAssert_1__reg.values[0];}
  { T0.values[0] = TERNARY(reset.values[0], 0xffffffffL, TesterTest_HWAssert_1__io_in.values[0]);}
  val_t T1;
  T1 = TesterTest_HWAssert_1__reg.values[0] != 0xff000000L;
  val_t T2;
  { T2 = T1 | reset.values[0];}
  ASSERT(T2 || reset.lo_word(), "Assertion Test");
}
void TesterTest_HWAssert_1_t::clock_hi ( dat_t<1> reset ) {
  dat_t<32> TesterTest_HWAssert_1__reg__shadow = T0;
  TesterTest_HWAssert_1__reg = T0;
}
void TesterTest_HWAssert_1_api_t::init_mapping_table (  ) {
  dat_table.clear();
  mem_table.clear();
  TesterTest_HWAssert_1_t* mod_typed = dynamic_cast<TesterTest_HWAssert_1_t*>(module);
  assert(mod_typed);
  dat_table["TesterTest_HWAssert_1.io_out"] = new dat_api<32>(&mod_typed->TesterTest_HWAssert_1__io_out, "TesterTest_HWAssert_1.io_out", "");
  dat_table["TesterTest_HWAssert_1.io_in"] = new dat_api<32>(&mod_typed->TesterTest_HWAssert_1__io_in, "TesterTest_HWAssert_1.io_in", "");
  dat_table["TesterTest_HWAssert_1.reg"] = new dat_api<32>(&mod_typed->TesterTest_HWAssert_1__reg, "TesterTest_HWAssert_1.reg", "");
}
