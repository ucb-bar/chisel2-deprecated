#ifndef __DataSuite_BypassDataComp_1__
#define __DataSuite_BypassDataComp_1__

#include "emulator.h"

class DataSuite_BypassDataComp_1_t : public mod_t {
 public:
  dat_t<3> T0;
  dat_t<1> DataSuite_BypassDataComp_1__io_valid_2;
  dat_t<3> DataSuite_BypassDataComp_1__io_data;
  dat_t<1> DataSuite_BypassDataComp_1__io_valid_1;
  dat_t<1> DataSuite_BypassDataComp_1__io_valid_0;
  int clk;
  int clk_cnt;

  void init ( bool rand_init = false );
  void clock_lo_clk ( dat_t<1> reset );
  void clock_hi_clk ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  void print ( FILE* f );
  bool scan ( FILE* f );
  void dump ( FILE* f, int t );
};

#endif
