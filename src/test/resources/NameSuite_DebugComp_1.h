#ifndef __NameSuite_DebugComp_1__
#define __NameSuite_DebugComp_1__

#include "emulator.h"

class NameSuite_DebugComp_1_t : public mod_t {
 public:
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_wb_wen;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_wb_wen__prev;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_out;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_out__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_out;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_out__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev;
  int NameSuite_DebugComp_1__clk;
  int NameSuite_DebugComp_1__clk_cnt;

  void init ( bool rand_init = false );
  void clock_lo_NameSuite_DebugComp_1__clk ( dat_t<1> reset );
  void clock_hi_NameSuite_DebugComp_1__clk ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  void print ( FILE* f );
  bool scan ( FILE* f );
  void dump ( FILE* f, int t );
};

#endif
