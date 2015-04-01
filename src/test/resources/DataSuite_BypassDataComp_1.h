#ifndef __DataSuite_BypassDataComp_1__
#define __DataSuite_BypassDataComp_1__

#include "emulator.h"

class DataSuite_BypassDataComp_1_t : public mod_t {
 private:
  val_t __rand_seed;
  void __srand(val_t seed) { __rand_seed = seed; }
  val_t __rand_val() { return ::__rand_val(&__rand_seed); }
 public:
  dat_t<1> DataSuite_BypassDataComp_1__io_valid_0;
  dat_t<1> DataSuite_BypassDataComp_1__io_valid_1;
  dat_t<1> DataSuite_BypassDataComp_1__io_valid_2;
  dat_t<3> DataSuite_BypassDataComp_1__io_data;
  int clk;
  int clk_cnt;

  void init ( val_t rand_init = 0 );
  void clock_lo ( dat_t<1> reset );
  void clock_hi ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  mod_t* clone();
  bool set_circuit_from(mod_t* src);
  void print ( FILE* f );
  void print ( std::ostream& s );
  void dump ( FILE* f, int t );
  void dump_init ( FILE* f );

};

class DataSuite_BypassDataComp_1_api_t : public mod_api_t {
  void init_mapping_table();
};



#endif
