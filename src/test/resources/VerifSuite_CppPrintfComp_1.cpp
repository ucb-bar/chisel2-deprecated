#include "VerifSuite_CppPrintfComp_1.h"

void VerifSuite_CppPrintfComp_1_t::init ( val_t rand_init ) {
  this->__srand(rand_init);
}
int VerifSuite_CppPrintfComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_hi( reset );
  if (clk_cnt == 0) clock_lo( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}
mod_t* VerifSuite_CppPrintfComp_1_t::clone() {
  mod_t* cloned = new VerifSuite_CppPrintfComp_1_t(*this);
  return cloned;
}
bool VerifSuite_CppPrintfComp_1_t::set_circuit_from(mod_t* src) {
  VerifSuite_CppPrintfComp_1_t* mod_typed = dynamic_cast<VerifSuite_CppPrintfComp_1_t*>(src);
  assert(mod_typed);
  VerifSuite_CppPrintfComp_1__io_y = mod_typed->VerifSuite_CppPrintfComp_1__io_y;
  VerifSuite_CppPrintfComp_1__io_x = mod_typed->VerifSuite_CppPrintfComp_1__io_x;
  VerifSuite_CppPrintfComp_1__io_z = mod_typed->VerifSuite_CppPrintfComp_1__io_z;
  T1 = mod_typed->T1;
  T2 = mod_typed->T2;
  T3 = mod_typed->T3;
  T4 = mod_typed->T4;
  clk = mod_typed->clk;
  clk_cnt = mod_typed->clk_cnt;
  return true;
}
void VerifSuite_CppPrintfComp_1_t::print ( FILE* f ) {
#if __cplusplus >= 201103L
  if (T1.values[0]) dat_fprintf<104>(f, "display %h %h", T3, T2);
#endif
fflush(f);
}
void VerifSuite_CppPrintfComp_1_t::print ( std::ostream& s ) {
#if __cplusplus >= 201103L
  if (T1.values[0]) dat_prints<104>(s, "display %h %h", T3, T2);
#endif
s.flush();
}
void VerifSuite_CppPrintfComp_1_t::dump_init(FILE *f) {
}
void VerifSuite_CppPrintfComp_1_t::dump(FILE *f, int t) {
}
void VerifSuite_CppPrintfComp_1_t::clock_lo ( dat_t<1> reset ) {
  val_t T0;
  { T0 = VerifSuite_CppPrintfComp_1__io_y.values[0] | VerifSuite_CppPrintfComp_1__io_x.values[0] << 8;}
  { VerifSuite_CppPrintfComp_1__io_z.values[0] = T0;}
  { T1.values[0] = reset.values[0] ^ 0x1L;}
  { T2.values[0] = VerifSuite_CppPrintfComp_1__io_y.values[0];}
  { T3.values[0] = VerifSuite_CppPrintfComp_1__io_x.values[0];}
}
void VerifSuite_CppPrintfComp_1_t::clock_hi ( dat_t<1> reset ) {
}
void VerifSuite_CppPrintfComp_1_api_t::init_mapping_table() {
  dat_table.clear();
  mem_table.clear();
  VerifSuite_CppPrintfComp_1_t* mod_typed = dynamic_cast<VerifSuite_CppPrintfComp_1_t*>(module);
  assert(mod_typed);
  dat_table["VerifSuite_CppPrintfComp_1.io_y"] = new dat_api<8>(&mod_typed->VerifSuite_CppPrintfComp_1__io_y, "VerifSuite_CppPrintfComp_1.io_y", "");
  dat_table["VerifSuite_CppPrintfComp_1.io_x"] = new dat_api<8>(&mod_typed->VerifSuite_CppPrintfComp_1__io_x, "VerifSuite_CppPrintfComp_1.io_x", "");
  dat_table["VerifSuite_CppPrintfComp_1.io_z"] = new dat_api<16>(&mod_typed->VerifSuite_CppPrintfComp_1__io_z, "VerifSuite_CppPrintfComp_1.io_z", "");
}
