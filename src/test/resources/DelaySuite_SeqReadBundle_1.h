#ifndef __DelaySuite_SeqReadBundle_1__
#define __DelaySuite_SeqReadBundle_1__

#include "emulator.h"

class DelaySuite_SeqReadBundle_1_t : public mod_t {
 private:
  val_t __rand_seed;
  void __srand(val_t seed) { __rand_seed = seed; }
  val_t __rand_val() { return ::__rand_val(&__rand_seed); }
 public:
  dat_t<1> DelaySuite_SeqReadBundle_1__io_ren;
  dat_t<1> DelaySuite_SeqReadBundle_1__io_wen;
  dat_t<4> DelaySuite_SeqReadBundle_1__io_raddr;
  dat_t<4> T6;
  dat_t<4> DelaySuite_SeqReadBundle_1__R9;
  dat_t<4> DelaySuite_SeqReadBundle_1__io_waddr;
  dat_t<8> DelaySuite_SeqReadBundle_1__io_out_1_a_a;
  dat_t<8> DelaySuite_SeqReadBundle_1__io_out_0_a_a;
  dat_t<8> DelaySuite_SeqReadBundle_1__io_in_0_a_a;
  dat_t<8> DelaySuite_SeqReadBundle_1__io_in_1_a_a;
  dat_t<16> DelaySuite_SeqReadBundle_1__io_out_1_a_b;
  dat_t<16> DelaySuite_SeqReadBundle_1__io_out_0_a_b;
  dat_t<16> DelaySuite_SeqReadBundle_1__io_in_0_a_b;
  dat_t<16> DelaySuite_SeqReadBundle_1__io_in_1_a_b;
  dat_t<32> DelaySuite_SeqReadBundle_1__io_out_1_a_b_;
  dat_t<32> DelaySuite_SeqReadBundle_1__io_in_0_a_b_;
  dat_t<32> DelaySuite_SeqReadBundle_1__io_in_1_a_b_;
  dat_t<32> DelaySuite_SeqReadBundle_1__io_out_0_a_b_;
  dat_t<112> T12;
  mem_t<112,16> DelaySuite_SeqReadBundle_1__mem;
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
