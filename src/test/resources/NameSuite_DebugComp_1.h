#ifndef __NameSuite_DebugComp_1__
#define __NameSuite_DebugComp_1__

#include "emulator.h"

class NameSuite_DebugComp_1_t : public mod_t {
 public:
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_wb_wen;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_wen;
  dat_t<1> reset;
  dat_t<1> NameSuite_DebugComp_1_dpath__reset;
  dat_t<1> T0;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_out;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_out;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_wb_wen__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__reset__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_out__prev;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_out__prev;
  int clk;
  int clk_cnt;

  void init ( bool rand_init = false );
  void clock_lo ( dat_t<1> reset );
  void clock_hi ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  mod_t* clone();
  bool set_circuit_from(mod_t* src);
  void print ( FILE* f );
  void dump ( FILE* f, int t );
  void dump_init ( FILE* f );
};

class NameSuite_DebugComp_1_api_t : public mod_api_t {
  void init_mapping_table();
};



#endif
