#include "WhenSuite_SwitchClassComp_1.h"

void WhenSuite_SwitchClassComp_1_t::init ( val_t rand_init ) {
  nodes.clear();
  mems.clear();
  nodes.push_back(debug_node_t("WhenSuite_SwitchClassComp_1.io_in0", &WhenSuite_SwitchClassComp_1__io_in0));
  nodes.push_back(debug_node_t("WhenSuite_SwitchClassComp_1.io_in1", &WhenSuite_SwitchClassComp_1__io_in1));
  nodes.push_back(debug_node_t("WhenSuite_SwitchClassComp_1.io_out", &WhenSuite_SwitchClassComp_1__io_out));
}
int WhenSuite_SwitchClassComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_lo( reset );
  if (clk_cnt == 0) clock_hi( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}
void WhenSuite_SwitchClassComp_1_t::print ( FILE* f, FILE* e ) {
}
bool WhenSuite_SwitchClassComp_1_t::scan ( FILE* f ) {
  return(!feof(f));
}
void WhenSuite_SwitchClassComp_1_t::dump(FILE *f, int t) {
}
void WhenSuite_SwitchClassComp_1_t::clock_lo ( dat_t<1> reset ) {
  val_t T0__w0;
  { T0__w0 = WhenSuite_SwitchClassComp_1__io_in0.values[0]&0xf3L; }
  val_t T1__w0;
  T1__w0 = T0__w0 == 0x51L;
  val_t T2__w0;
  { T2__w0 = TERNARY(T1__w0, WhenSuite_SwitchClassComp_1__io_in1.values[0], WhenSuite_SwitchClassComp_1__io_in0.values[0]); }
  { WhenSuite_SwitchClassComp_1__io_out.values[0] = T2__w0; }
}
void WhenSuite_SwitchClassComp_1_t::clock_hi ( dat_t<1> reset ) {
}
