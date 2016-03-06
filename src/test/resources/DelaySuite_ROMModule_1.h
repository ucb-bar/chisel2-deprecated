#ifndef __DelaySuite_ROMModule_1__
#define __DelaySuite_ROMModule_1__

#include "emulator.h"

class DelaySuite_ROMModule_1_t : public mod_t {
 private:
  val_t __rand_seed;
  void __srand(val_t seed) { __rand_seed = seed; }
  val_t __rand_val() { return ::__rand_val(&__rand_seed); }
 public:
  dat_t<2> DelaySuite_ROMModule_1__io_addr;
  dat_t<4> DelaySuite_ROMModule_1__io_out;
  mem_t<4,3> T1;
  clk_t clk;

  void init ( val_t rand_init = 0 );
  void clock_lo ( dat_t<1> reset, bool assert_fire=true );
  void clock_hi ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  void print ( FILE* f );
  void print ( std::ostream& s );
  void dump ( FILE* f, int t, dat_t<1> reset=LIT<1>(0) );
  void dump_init ( FILE* f );

};



#endif
