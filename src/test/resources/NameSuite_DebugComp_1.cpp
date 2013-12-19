#include "NameSuite_DebugComp_1.h"

void NameSuite_DebugComp_1_t::init ( bool rand_init ) {
  if (rand_init) NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.randomize();
}
void NameSuite_DebugComp_1_t::clock_lo_NameSuite_DebugComp_1__clk ( dat_t<1> reset ) {
  { NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0] = NameSuite_DebugComp_1__io_ctrl_wb_wen.values[0]; }
  val_t NameSuite_DebugComp_1_dpath__wb_wen__w0;
  { NameSuite_DebugComp_1_dpath__wb_wen__w0 = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0]||NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]; }
  val_t T0__w0;
  { val_t __mask = -NameSuite_DebugComp_1_dpath__wb_wen__w0; T0__w0 = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0] ^ ((NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0] ^ NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0]) & __mask); }
  { NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow.values[0] = TERNARY(reset.values[0], 0x0L, T0__w0); }
  { NameSuite_DebugComp_1_dpath__io_ctrl_out.values[0] = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]; }
  { NameSuite_DebugComp_1__io_ctrl_out.values[0] = NameSuite_DebugComp_1_dpath__io_ctrl_out.values[0]; }
}
void NameSuite_DebugComp_1_t::clock_hi_NameSuite_DebugComp_1__clk ( dat_t<1> reset ) {
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow;
}
int NameSuite_DebugComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (NameSuite_DebugComp_1__clk_cnt < min) min = NameSuite_DebugComp_1__clk_cnt;
  NameSuite_DebugComp_1__clk_cnt-=min;
  if (NameSuite_DebugComp_1__clk_cnt == 0) clock_lo_NameSuite_DebugComp_1__clk( reset );
  if (NameSuite_DebugComp_1__clk_cnt == 0) clock_hi_NameSuite_DebugComp_1__clk( reset );
  if (NameSuite_DebugComp_1__clk_cnt == 0) NameSuite_DebugComp_1__clk_cnt = NameSuite_DebugComp_1__clk;
  return min;
}
void NameSuite_DebugComp_1_t::print ( FILE* f ) {
}
bool NameSuite_DebugComp_1_t::scan ( FILE* f ) {
  return(!feof(f));
}
void NameSuite_DebugComp_1_t::dump(FILE *f, int t) {
  if (t == 0) {
    fprintf(f, "$timescale 1ps $end\n");
    fprintf(f, "$scope module NameSuite_DebugComp_1 $end\n");
    fprintf(f, "$var wire 1 N0 io_ctrl_wb_wen $end\n");
    fprintf(f, "$var wire 1 N1 io_ctrl_out $end\n");
    fprintf(f, "$var wire 1 N5 reset $end\n");
    fprintf(f, "$scope module dpath $end\n");
    fprintf(f, "$var wire 1 N2 io_ctrl_out $end\n");
    fprintf(f, "$var wire 1 N3 wb_reg_ll_wb $end\n");
    fprintf(f, "$var wire 1 N4 io_ctrl_wb_wen $end\n");
    fprintf(f, "$upscope $end\n");
    fprintf(f, "$upscope $end\n");
    fprintf(f, "$enddefinitions $end\n");
    fprintf(f, "$dumpvars\n");
    fprintf(f, "$end\n");
  }
  fprintf(f, "#%d\n", t);
  if (t == 0 || (NameSuite_DebugComp_1__io_ctrl_wb_wen != NameSuite_DebugComp_1__io_ctrl_wb_wen__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1__io_ctrl_wb_wen, "N0");
  NameSuite_DebugComp_1__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1__io_ctrl_wb_wen;
  if (t == 0 || (NameSuite_DebugComp_1__io_ctrl_out != NameSuite_DebugComp_1__io_ctrl_out__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1__io_ctrl_out, "N1");
  NameSuite_DebugComp_1__io_ctrl_out__prev = NameSuite_DebugComp_1__io_ctrl_out;
  if (t == 0 || (NameSuite_DebugComp_1_dpath__io_ctrl_out != NameSuite_DebugComp_1_dpath__io_ctrl_out__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__io_ctrl_out, "N2");
  NameSuite_DebugComp_1_dpath__io_ctrl_out__prev = NameSuite_DebugComp_1_dpath__io_ctrl_out;
  if (t == 0 || (NameSuite_DebugComp_1_dpath__wb_reg_ll_wb != NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__wb_reg_ll_wb, "N3");
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  if (t == 0 || (NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen != NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen, "N4");
  NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
}
