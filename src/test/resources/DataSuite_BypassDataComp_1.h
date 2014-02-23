#ifndef __DataSuite_BypassDataComp_1__
#define __DataSuite_BypassDataComp_1__

#include "emulator.h"

class DataSuite_BypassDataComp_1_t : public mod_t {
 public:
  dat_t<0> DataSuite_BypassDataComp_1__io_valid;
  int clk;
  int clk_cnt;

  void init ( bool rand_init = false );
  void clock_lo ( dat_t<1> reset );
  void clock_hi ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  void print ( FILE* f, FILE* e);
  bool scan ( FILE* f );
  void dump ( FILE* f, int t );
};



#endif
