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
void NameSuite_DebugComp_1_t::print ( FILE* f ) {
}
void NameSuite_DebugComp_1_t::dump(FILE *f, int t) {
  if (t == 0) {
    fprintf(f, "$timescale 1ps $end\n");
    fprintf(f, "$scope module NameSuite_DebugComp_1 $end\n");
    fprintf(f, "$var wire 1 N0 reset $end\n");
    fprintf(f, "$var wire 1 N2 io_ctrl_wb_wen $end\n");
    fprintf(f, "$var wire 1 N6 io_ctrl_out $end\n");
    fprintf(f, "$scope module dpath $end\n");
    fprintf(f, "$var wire 1 N1 reset $end\n");
    fprintf(f, "$var wire 1 N3 io_ctrl_wb_wen $end\n");
    fprintf(f, "$var wire 1 N4 wb_reg_ll_wb $end\n");
    fprintf(f, "$var wire 1 N5 io_ctrl_out $end\n");
    fprintf(f, "$upscope $end\n");
    fprintf(f, "$upscope $end\n");
    fprintf(f, "$enddefinitions $end\n");
    fprintf(f, "$dumpvars\n");
    fprintf(f, "$end\n");
  }
  fprintf(f, "#%d\n", t);
  if (t == 0 || (NameSuite_DebugComp_1_dpath__reset != NameSuite_DebugComp_1_dpath__reset__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__reset, "N1");
  NameSuite_DebugComp_1_dpath__reset__prev = NameSuite_DebugComp_1_dpath__reset;
  if (t == 0 || (NameSuite_DebugComp_1__io_ctrl_wb_wen != NameSuite_DebugComp_1__io_ctrl_wb_wen__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1__io_ctrl_wb_wen, "N2");
  NameSuite_DebugComp_1__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1__io_ctrl_wb_wen;
  if (t == 0 || (NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen != NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen, "N3");
  NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  if (t == 0 || (NameSuite_DebugComp_1_dpath__wb_reg_ll_wb != NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__wb_reg_ll_wb, "N4");
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  if (t == 0 || (NameSuite_DebugComp_1_dpath__io_ctrl_out != NameSuite_DebugComp_1_dpath__io_ctrl_out__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__io_ctrl_out, "N5");
  NameSuite_DebugComp_1_dpath__io_ctrl_out__prev = NameSuite_DebugComp_1_dpath__io_ctrl_out;
  if (t == 0 || (NameSuite_DebugComp_1__io_ctrl_out != NameSuite_DebugComp_1__io_ctrl_out__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1__io_ctrl_out, "N6");
  NameSuite_DebugComp_1__io_ctrl_out__prev = NameSuite_DebugComp_1__io_ctrl_out;
}
void NameSuite_DebugComp_1_t::clock_lo ( dat_t<1> reset ) {
  { NameSuite_DebugComp_1_dpath__reset.values[0] = reset.values[0]; }
  { NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0] = NameSuite_DebugComp_1__io_ctrl_wb_wen.values[0]; }
  { NameSuite_DebugComp_1_dpath__wb_wen.values[0] = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0]||NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]; }
  val_t T0__w0;
  { T0__w0 = TERNARY(NameSuite_DebugComp_1_dpath__wb_wen.values[0], NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0], NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]); }
  { NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow.values[0] = TERNARY(NameSuite_DebugComp_1_dpath__reset.values[0], 0x0L, T0__w0); }
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
