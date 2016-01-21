#ifndef __ConnectSuite_UnconnectedIOs_1__
#define __ConnectSuite_UnconnectedIOs_1__

#include "emulator.h"

class ConnectSuite_UnconnectedIOs_1_t : public mod_t {
 private:
  val_t __rand_seed;
  void __srand(val_t seed) { __rand_seed = seed; }
  val_t __rand_val() { return ::__rand_val(&__rand_seed); }
 public:
  dat_t<1> ConnectSuite_UnconnectedIOs_1__io_in;
  dat_t<1> ConnectSuite_UnconnectedIOs_1_sub__io_in;
  dat_t<1> ConnectSuite_UnconnectedIOs_1_sub__reset;
  dat_t<1> T0;
  dat_t<1> ConnectSuite_UnconnectedIOs_1_sub__r;
  dat_t<1> ConnectSuite_UnconnectedIOs_1_sub__io_ncOut;
  dat_t<1> ConnectSuite_UnconnectedIOs_1_sub__io_out;
  dat_t<1> ConnectSuite_UnconnectedIOs_1__io_out;
  dat_t<1> ConnectSuite_UnconnectedIOs_1__io_ncOut;
  dat_t<1> reset;
  dat_t<1> ConnectSuite_UnconnectedIOs_1__regs_0;
  dat_t<1> ConnectSuite_UnconnectedIOs_1__regs_1;
  dat_t<1> ConnectSuite_UnconnectedIOs_1__regs_2;
  dat_t<1> ConnectSuite_UnconnectedIOs_1__io_ncIn;
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
