#ifndef __DelaySuite_ROMModule_1__
#define __DelaySuite_ROMModule_1__

#include "emulator.h"

class DelaySuite_ROMModule_1_t : public mod_t {
 public:
  mem_t<4,3> T0;
  dat_t<2> DelaySuite_ROMModule_1__io_addr;
  dat_t<4> DelaySuite_ROMModule_1__io_out;
  int clk;
  int clk_cnt;

  void init ( bool rand_init = false );
  void clock_lo ( dat_t<1> reset );
  void clock_hi ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  void print ( FILE* f );
  void dump ( FILE* f, int t );
  void dump_init ( FILE* f );
};

class DelaySuite_ROMModule_1_api_t : public mod_api_t {
  void init_mapping_table();
};



#endif
