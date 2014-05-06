#include "NameSuite_DebugComp_1.h"

void NameSuite_DebugComp_1_t::init ( bool rand_init ) {
  if (rand_init) NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.randomize();
}
int NameSuite_DebugComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_lo( reset );
  if (clk_cnt == 0) clock_hi( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}
mod_t* NameSuite_DebugComp_1_t::clone() {
  mod_t* cloned = new NameSuite_DebugComp_1_t(*this);
  return cloned;
}
bool NameSuite_DebugComp_1_t::set_circuit_from(mod_t* src) {
  NameSuite_DebugComp_1_t* mod_typed = dynamic_cast<NameSuite_DebugComp_1_t*>(src);
  assert(mod_typed);
  NameSuite_DebugComp_1__io_ctrl_wb_wen = mod_typed->NameSuite_DebugComp_1__io_ctrl_wb_wen;
  NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen = mod_typed->NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  NameSuite_DebugComp_1_dpath__wb_wen = mod_typed->NameSuite_DebugComp_1_dpath__wb_wen;
  NameSuite_DebugComp_1_dpath__reset = mod_typed->NameSuite_DebugComp_1_dpath__reset;
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb = mod_typed->NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow = mod_typed->NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow;
  NameSuite_DebugComp_1_dpath__io_ctrl_out = mod_typed->NameSuite_DebugComp_1_dpath__io_ctrl_out;
  NameSuite_DebugComp_1__io_ctrl_out = mod_typed->NameSuite_DebugComp_1__io_ctrl_out;
  clk = mod_typed->clk;
  clk_cnt = mod_typed->clk_cnt;
  return true;
}
void NameSuite_DebugComp_1_t::print ( FILE* f ) {
}
void NameSuite_DebugComp_1_t::dump_init(FILE *f) {
  fputs("$timescale 1ps $end\n", f);
  fputs("$scope module NameSuite_DebugComp_1 $end\n", f);
  fputs("$var wire 1 \x21 io_ctrl_wb_wen $end\n", f);
  fputs("$var wire 1 \x26 io_ctrl_out $end\n", f);
  fputs("$scope module dpath $end\n", f);
  fputs("$var wire 1 \x22 io_ctrl_wb_wen $end\n", f);
  fputs("$var wire 1 \x23 reset $end\n", f);
  fputs("$var wire 1 \x24 wb_reg_ll_wb $end\n", f);
  fputs("$var wire 1 \x25 io_ctrl_out $end\n", f);
  fputs("$upscope $end\n", f);
  fputs("$upscope $end\n", f);
  fputs("$enddefinitions $end\n", f);
  fputs("$dumpvars\n", f);
  fputs("$end\n", f);
  fputs("#0\n", f);
  dat_dump<1>(f, NameSuite_DebugComp_1__io_ctrl_wb_wen, 0x21);
  NameSuite_DebugComp_1__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1__io_ctrl_wb_wen;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen, 0x22);
  NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__reset, 0x23);
  NameSuite_DebugComp_1_dpath__reset__prev = NameSuite_DebugComp_1_dpath__reset;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__wb_reg_ll_wb, 0x24);
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__io_ctrl_out, 0x25);
  NameSuite_DebugComp_1_dpath__io_ctrl_out__prev = NameSuite_DebugComp_1_dpath__io_ctrl_out;
  dat_dump<1>(f, NameSuite_DebugComp_1__io_ctrl_out, 0x26);
  NameSuite_DebugComp_1__io_ctrl_out__prev = NameSuite_DebugComp_1__io_ctrl_out;
}
void NameSuite_DebugComp_1_t::dump(FILE *f, int t) {
  if (t == 0) return dump_init(f);
  fprintf(f, "#%d\n", t);
  if (NameSuite_DebugComp_1__io_ctrl_wb_wen != NameSuite_DebugComp_1__io_ctrl_wb_wen__prev)
    goto L0;
K0:
  if (NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen != NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev)
    goto L1;
K1:
  if (NameSuite_DebugComp_1_dpath__reset != NameSuite_DebugComp_1_dpath__reset__prev)
    goto L2;
K2:
  if (NameSuite_DebugComp_1_dpath__wb_reg_ll_wb != NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev)
    goto L3;
K3:
  if (NameSuite_DebugComp_1_dpath__io_ctrl_out != NameSuite_DebugComp_1_dpath__io_ctrl_out__prev)
    goto L4;
K4:
  if (NameSuite_DebugComp_1__io_ctrl_out != NameSuite_DebugComp_1__io_ctrl_out__prev)
    goto L5;
K5:
  return;
L0:
  NameSuite_DebugComp_1__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1__io_ctrl_wb_wen;
  dat_dump<1>(f, NameSuite_DebugComp_1__io_ctrl_wb_wen, 0x21);
  goto K0;
L1:
  NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen, 0x22);
  goto K1;
L2:
  NameSuite_DebugComp_1_dpath__reset__prev = NameSuite_DebugComp_1_dpath__reset;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__reset, 0x23);
  goto K2;
L3:
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__wb_reg_ll_wb, 0x24);
  goto K3;
L4:
  NameSuite_DebugComp_1_dpath__io_ctrl_out__prev = NameSuite_DebugComp_1_dpath__io_ctrl_out;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__io_ctrl_out, 0x25);
  goto K4;
L5:
  NameSuite_DebugComp_1__io_ctrl_out__prev = NameSuite_DebugComp_1__io_ctrl_out;
  dat_dump<1>(f, NameSuite_DebugComp_1__io_ctrl_out, 0x26);
  goto K5;
}
void NameSuite_DebugComp_1_t::clock_lo ( dat_t<1> reset ) {
  { NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0] = NameSuite_DebugComp_1__io_ctrl_wb_wen.values[0]; }
  { NameSuite_DebugComp_1_dpath__wb_wen.values[0] = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0]|NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]; }
  val_t T0__w0;
  { T0__w0 = TERNARY(NameSuite_DebugComp_1_dpath__wb_wen.values[0], NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0], NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]); }
  { NameSuite_DebugComp_1_dpath__reset.values[0] = reset.values[0]; }
  val_t T1__w0;
  { T1__w0 = TERNARY(NameSuite_DebugComp_1_dpath__reset.values[0], 0x0L, T0__w0); }
  { NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow.values[0] = T1__w0; }
  { NameSuite_DebugComp_1_dpath__io_ctrl_out.values[0] = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]; }
  { NameSuite_DebugComp_1__io_ctrl_out.values[0] = NameSuite_DebugComp_1_dpath__io_ctrl_out.values[0]; }
}
void NameSuite_DebugComp_1_t::clock_hi ( dat_t<1> reset ) {
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow;
}
void NameSuite_DebugComp_1_api_t::init_mapping_table() {
  dat_table.clear();
  mem_table.clear();
  NameSuite_DebugComp_1_t* mod_typed = dynamic_cast<NameSuite_DebugComp_1_t*>(module);
  assert(mod_typed);
  dat_table["NameSuite_DebugComp_1.io_ctrl_wb_wen"] = new dat_api<1>(&mod_typed->NameSuite_DebugComp_1__io_ctrl_wb_wen, "NameSuite_DebugComp_1.io_ctrl_wb_wen", "");
  dat_table["NameSuite_DebugComp_1.dpath.io_ctrl_wb_wen"] = new dat_api<1>(&mod_typed->NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen, "NameSuite_DebugComp_1.dpath.io_ctrl_wb_wen", "");
  dat_table["NameSuite_DebugComp_1.dpath.wb_wen"] = new dat_api<1>(&mod_typed->NameSuite_DebugComp_1_dpath__wb_wen, "NameSuite_DebugComp_1.dpath.wb_wen", "");
  dat_table["NameSuite_DebugComp_1.dpath.wb_reg_ll_wb"] = new dat_api<1>(&mod_typed->NameSuite_DebugComp_1_dpath__wb_reg_ll_wb, "NameSuite_DebugComp_1.dpath.wb_reg_ll_wb", "");
  dat_table["NameSuite_DebugComp_1.dpath.io_ctrl_out"] = new dat_api<1>(&mod_typed->NameSuite_DebugComp_1_dpath__io_ctrl_out, "NameSuite_DebugComp_1.dpath.io_ctrl_out", "");
  dat_table["NameSuite_DebugComp_1.io_ctrl_out"] = new dat_api<1>(&mod_typed->NameSuite_DebugComp_1__io_ctrl_out, "NameSuite_DebugComp_1.io_ctrl_out", "");
}
