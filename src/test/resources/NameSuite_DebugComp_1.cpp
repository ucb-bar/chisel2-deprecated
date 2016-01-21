#include "NameSuite_DebugComp_1.h"

void NameSuite_DebugComp_1_t::init ( val_t rand_init ) {
  this->__srand(rand_init);
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.randomize(&__rand_seed);
  clk.len = 1;
  clk.cnt = 0;
  clk.values[0] = 0;
}
int NameSuite_DebugComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk.cnt < min) min = clk.cnt;
  clk.cnt-=min;
  if (clk.cnt == 0) clock_lo( reset );
  mod_t::dump( reset );
  if (clk.cnt == 0) clock_hi( reset );
  if (clk.cnt == 0) clk.cnt = clk.len;
  return min;
}
void NameSuite_DebugComp_1_t::print ( FILE* f ) {
}
void NameSuite_DebugComp_1_t::print ( std::ostream& s ) {
}
void NameSuite_DebugComp_1_t::dump_init ( FILE* f ) {
  fputs("$timescale 1ps $end\n", f);
  fputs("$scope module NameSuite_DebugComp_1 $end\n", f);
  fputs("$var wire 1 \x21 clk $end\n", f);
  fputs("$var wire 1 \x22 reset $end\n", f);
  fputs("$var wire 1 \x23 io_ctrl_wb_wen $end\n", f);
  fputs("$var wire 1 \x28 io_ctrl_out $end\n", f);
  fputs("$scope module dpath $end\n", f);
  fputs("$var wire 1 \x24 io_ctrl_wb_wen $end\n", f);
  fputs("$var wire 1 \x25 reset $end\n", f);
  fputs("$var wire 1 \x26 wb_reg_ll_wb $end\n", f);
  fputs("$var wire 1 \x27 io_ctrl_out $end\n", f);
  fputs("$upscope $end\n", f);
  fputs("$upscope $end\n", f);
  fputs("$enddefinitions $end\n", f);
  fputs("$dumpvars\n", f);
  fputs("$end\n", f);
  fputs("#0\n", f);
  if (clk.cnt == 0) {
    clk.values[0] = 1;
    dat_dump<1>(f, clk, 0x21);
  }
  dat_t<1> reset = LIT<1>(1);
  dat_dump<1>(f, reset, 0x22);
  dat_dump<1>(f, NameSuite_DebugComp_1__io_ctrl_wb_wen, 0x23);
  NameSuite_DebugComp_1__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1__io_ctrl_wb_wen;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen, 0x24);
  NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__reset, 0x25);
  NameSuite_DebugComp_1_dpath__reset__prev = NameSuite_DebugComp_1_dpath__reset;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__wb_reg_ll_wb, 0x26);
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__io_ctrl_out, 0x27);
  NameSuite_DebugComp_1_dpath__io_ctrl_out__prev = NameSuite_DebugComp_1_dpath__io_ctrl_out;
  dat_dump<1>(f, NameSuite_DebugComp_1__io_ctrl_out, 0x28);
  NameSuite_DebugComp_1__io_ctrl_out__prev = NameSuite_DebugComp_1__io_ctrl_out;
  fputs("#1\n", f);
  if (clk.cnt == 0) {
    clk.values[0] = 0;
    dat_dump<1>(f, clk, 0x21);
  }
}
void NameSuite_DebugComp_1_t::dump ( FILE* f, int t, dat_t<1> reset ) {
  if (t == 0) return dump_init(f);
  fprintf(f, "#%d\n", t << 1);
  if (clk.cnt == 0)  goto L0;
K0:  if (reset != reset__prev)  goto L1;
K1:  if (NameSuite_DebugComp_1__io_ctrl_wb_wen != NameSuite_DebugComp_1__io_ctrl_wb_wen__prev)  goto L2;
K2:  if (NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen != NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev)  goto L3;
K3:  if (NameSuite_DebugComp_1_dpath__reset != NameSuite_DebugComp_1_dpath__reset__prev)  goto L4;
K4:  if (NameSuite_DebugComp_1_dpath__wb_reg_ll_wb != NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev)  goto L5;
K5:  if (NameSuite_DebugComp_1_dpath__io_ctrl_out != NameSuite_DebugComp_1_dpath__io_ctrl_out__prev)  goto L6;
K6:  if (NameSuite_DebugComp_1__io_ctrl_out != NameSuite_DebugComp_1__io_ctrl_out__prev)  goto L7;
K7:  fprintf(f, "#%d\n", (t << 1) + 1);
  if (clk.cnt == 0)  goto Z0;
C0:  return;
L0:
  clk.values[0] = 1;
  dat_dump<1>(f, clk, 0x21);
  goto K0;
L1:
  reset__prev = reset;
  dat_dump<1>(f, reset, 0x22);
  goto K1;
L2:
  NameSuite_DebugComp_1__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1__io_ctrl_wb_wen;
  dat_dump<1>(f, NameSuite_DebugComp_1__io_ctrl_wb_wen, 0x23);
  goto K2;
L3:
  NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen, 0x24);
  goto K3;
L4:
  NameSuite_DebugComp_1_dpath__reset__prev = NameSuite_DebugComp_1_dpath__reset;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__reset, 0x25);
  goto K4;
L5:
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__wb_reg_ll_wb, 0x26);
  goto K5;
L6:
  NameSuite_DebugComp_1_dpath__io_ctrl_out__prev = NameSuite_DebugComp_1_dpath__io_ctrl_out;
  dat_dump<1>(f, NameSuite_DebugComp_1_dpath__io_ctrl_out, 0x27);
  goto K6;
L7:
  NameSuite_DebugComp_1__io_ctrl_out__prev = NameSuite_DebugComp_1__io_ctrl_out;
  dat_dump<1>(f, NameSuite_DebugComp_1__io_ctrl_out, 0x28);
  goto K7;
Z0:
  clk.values[0] = 0;
  dat_dump<1>(f, clk, 0x21);
  goto C0;
}
void NameSuite_DebugComp_1_t::clock_lo ( dat_t<1> reset, bool assert_fire ) {
  { NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0] = NameSuite_DebugComp_1__io_ctrl_wb_wen.values[0];}
  { NameSuite_DebugComp_1_dpath__wb_wen.values[0] = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0] | NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0];}
  val_t T0;
  { T0 = TERNARY_1(NameSuite_DebugComp_1_dpath__wb_wen.values[0], NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0], NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]);}
  { NameSuite_DebugComp_1_dpath__reset.values[0] = reset.values[0];}
  { T1.values[0] = TERNARY(NameSuite_DebugComp_1_dpath__reset.values[0], 0x0L, T0);}
  { NameSuite_DebugComp_1_dpath__io_ctrl_out.values[0] = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0];}
  { NameSuite_DebugComp_1__io_ctrl_out.values[0] = NameSuite_DebugComp_1_dpath__io_ctrl_out.values[0];}
}
void NameSuite_DebugComp_1_t::clock_hi ( dat_t<1> reset ) {
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__shadow = T1;
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb = T1;
}
