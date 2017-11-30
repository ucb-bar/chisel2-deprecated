#include "VerifSuite_CppPrintfComp_1.h"

void VerifSuite_CppPrintfComp_1_t::init ( val_t rand_init ) {
  this->__srand(rand_init);
  clk.len = 1;
  clk.cnt = 0;
  clk.values[0] = 0;
}
int VerifSuite_CppPrintfComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk.cnt < min) min = clk.cnt;
  clk.cnt-=min;
  if (clk.cnt == 0) clock_lo( reset );
  if (!reset.to_bool()) print( std::cerr );
  if (clk.cnt == 0) clock_hi( reset );
  if (clk.cnt == 0) clk.cnt = clk.len;
  return min;
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
void VerifSuite_CppPrintfComp_1_t::dump_init ( FILE* f ) {
}
void VerifSuite_CppPrintfComp_1_t::dump ( FILE* f, val_t t, dat_t<1> reset ) {
}
void VerifSuite_CppPrintfComp_1_t::clock_lo ( dat_t<1> reset, bool assert_fire ) {
  val_t T0;
  { T0 = VerifSuite_CppPrintfComp_1__io_y.values[0] | VerifSuite_CppPrintfComp_1__io_x.values[0] << 8;}
  { VerifSuite_CppPrintfComp_1__io_z.values[0] = T0;}
  { T1.values[0] = reset.values[0] ^ 0x1L;}
  { T2.values[0] = VerifSuite_CppPrintfComp_1__io_y.values[0];}
  { T3.values[0] = VerifSuite_CppPrintfComp_1__io_x.values[0];}
}
void VerifSuite_CppPrintfComp_1_t::clock_hi ( dat_t<1> reset ) {
}
