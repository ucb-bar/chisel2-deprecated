#ifndef __DataSuite_BypassDataComp_1__
#define __DataSuite_BypassDataComp_1__

#include "emulator.h"

class DataSuite_BypassDataComp_1_t : public mod_t {
 public:
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

class DataSuite_BypassDataComp_1_api_t : public mod_api_t {
  void init_mapping_table();
};



#endif
