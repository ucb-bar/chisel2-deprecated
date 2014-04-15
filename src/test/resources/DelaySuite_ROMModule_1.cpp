#include "DelaySuite_ROMModule_1.h"

void DelaySuite_ROMModule_1_t::init ( bool rand_init ) {
  { T0.put(0, 0, 0x1L); }
  { T0.put(1, 0, 0x2L); }
  { T0.put(2, 0, 0x3L); }
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
mod_t* DelaySuite_ROMModule_1_t::clone() {
  mod_t* cloned = new DelaySuite_ROMModule_1_t(*this);
  return cloned;
}
bool DelaySuite_ROMModule_1_t::set_circuit_from(mod_t* src) {
  DelaySuite_ROMModule_1_t* mod_typed = dynamic_cast<DelaySuite_ROMModule_1_t*>(src);
  assert(mod_typed);
  T0 = mod_typed->T0;
  DelaySuite_ROMModule_1__io_addr = mod_typed->DelaySuite_ROMModule_1__io_addr;
  DelaySuite_ROMModule_1__io_out = mod_typed->DelaySuite_ROMModule_1__io_out;
  clk = mod_typed->clk;
  clk_cnt = mod_typed->clk_cnt;
  return true;
}
void DelaySuite_ROMModule_1_t::print ( FILE* f ) {
}
void DelaySuite_ROMModule_1_t::dump_init(FILE *f) {
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
void DelaySuite_ROMModule_1_api_t::init_mapping_table() {
  dat_table.clear();
  mem_table.clear();
  DelaySuite_ROMModule_1_t* mod_typed = dynamic_cast<DelaySuite_ROMModule_1_t*>(module);
  assert(mod_typed);
  dat_table["DelaySuite_ROMModule_1.io_addr"] = new dat_api<2>(&mod_typed->DelaySuite_ROMModule_1__io_addr, "DelaySuite_ROMModule_1.io_addr", "");
  dat_table["DelaySuite_ROMModule_1.io_out"] = new dat_api<4>(&mod_typed->DelaySuite_ROMModule_1__io_out, "DelaySuite_ROMModule_1.io_out", "");
}
