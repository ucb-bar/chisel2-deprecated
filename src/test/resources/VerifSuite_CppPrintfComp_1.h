#ifndef __VerifSuite_CppPrintfComp_1__
#define __VerifSuite_CppPrintfComp_1__

#include "emulator.h"

class VerifSuite_CppPrintfComp_1_t : public mod_t {
 private:
  val_t __rand_seed;
  void __srand(val_t seed) { __rand_seed = seed; }
  val_t __rand_val() { return ::__rand_val(&__rand_seed); }
 public:
  dat_t<1> reset;
  dat_t<1> T1;
  dat_t<8> VerifSuite_CppPrintfComp_1__io_y;
  dat_t<8> VerifSuite_CppPrintfComp_1__io_x;
  dat_t<8> T2;
  dat_t<8> T3;
  dat_t<16> VerifSuite_CppPrintfComp_1__io_z;
  dat_t<104> T4;
  int clk;
  int clk_cnt;
  int last_dump_time;

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

class VerifSuite_CppPrintfComp_1_api_t : public mod_api_t {
  void init_mapping_table();
};



#endif
