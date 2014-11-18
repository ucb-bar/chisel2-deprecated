#include "DelaySuite_SeqReadBundle_1.h"

void DelaySuite_SeqReadBundle_1_t::init ( val_t rand_init ) {
  this->__srand(rand_init);
  R1.randomize(&__rand_seed);
  DelaySuite_SeqReadBundle_1__mem.randomize(&__rand_seed);
}
int DelaySuite_SeqReadBundle_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_hi( reset );
  if (clk_cnt == 0) clock_lo( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}
mod_t* DelaySuite_SeqReadBundle_1_t::clone() {
  mod_t* cloned = new DelaySuite_SeqReadBundle_1_t(*this);
  return cloned;
}
bool DelaySuite_SeqReadBundle_1_t::set_circuit_from ( mod_t* src ) {
  DelaySuite_SeqReadBundle_1_t* mod_typed = dynamic_cast<DelaySuite_SeqReadBundle_1_t*>(src);
  assert(mod_typed);
  DelaySuite_SeqReadBundle_1__io_out_1_a_a = mod_typed->DelaySuite_SeqReadBundle_1__io_out_1_a_a;
  DelaySuite_SeqReadBundle_1__io_out_1_a_b = mod_typed->DelaySuite_SeqReadBundle_1__io_out_1_a_b;
  DelaySuite_SeqReadBundle_1__io_out_1_a_b_ = mod_typed->DelaySuite_SeqReadBundle_1__io_out_1_a_b_;
  DelaySuite_SeqReadBundle_1__io_out_0_a_a = mod_typed->DelaySuite_SeqReadBundle_1__io_out_0_a_a;
  DelaySuite_SeqReadBundle_1__io_out_0_a_b = mod_typed->DelaySuite_SeqReadBundle_1__io_out_0_a_b;
  DelaySuite_SeqReadBundle_1__io_raddr = mod_typed->DelaySuite_SeqReadBundle_1__io_raddr;
  DelaySuite_SeqReadBundle_1__io_ren = mod_typed->DelaySuite_SeqReadBundle_1__io_ren;
  T0 = mod_typed->T0;
  R1 = mod_typed->R1;
  DelaySuite_SeqReadBundle_1__io_in_0_a_b_ = mod_typed->DelaySuite_SeqReadBundle_1__io_in_0_a_b_;
  DelaySuite_SeqReadBundle_1__io_in_0_a_b = mod_typed->DelaySuite_SeqReadBundle_1__io_in_0_a_b;
  DelaySuite_SeqReadBundle_1__io_in_0_a_a = mod_typed->DelaySuite_SeqReadBundle_1__io_in_0_a_a;
  DelaySuite_SeqReadBundle_1__io_in_1_a_b_ = mod_typed->DelaySuite_SeqReadBundle_1__io_in_1_a_b_;
  DelaySuite_SeqReadBundle_1__io_in_1_a_b = mod_typed->DelaySuite_SeqReadBundle_1__io_in_1_a_b;
  DelaySuite_SeqReadBundle_1__io_in_1_a_a = mod_typed->DelaySuite_SeqReadBundle_1__io_in_1_a_a;
  T2 = mod_typed->T2;
  DelaySuite_SeqReadBundle_1__io_wen = mod_typed->DelaySuite_SeqReadBundle_1__io_wen;
  DelaySuite_SeqReadBundle_1__io_waddr = mod_typed->DelaySuite_SeqReadBundle_1__io_waddr;
  DelaySuite_SeqReadBundle_1__mem = mod_typed->DelaySuite_SeqReadBundle_1__mem;
  DelaySuite_SeqReadBundle_1__io_out_0_a_b_ = mod_typed->DelaySuite_SeqReadBundle_1__io_out_0_a_b_;
  clk = mod_typed->clk;
  clk_cnt = mod_typed->clk_cnt;
  return true;
}
void DelaySuite_SeqReadBundle_1_t::print ( FILE* f ) {
}
void DelaySuite_SeqReadBundle_1_t::print ( std::ostream& s ) {
}
void DelaySuite_SeqReadBundle_1_t::dump_init ( FILE* f ) {
}
void DelaySuite_SeqReadBundle_1_t::dump ( FILE* f, int t ) {
}
void DelaySuite_SeqReadBundle_1_t::clock_lo ( dat_t<1> reset ) {
  val_t T3[2];
  { T3[0] = DelaySuite_SeqReadBundle_1__mem.get(R1.values[0], 0); T3[1] = DelaySuite_SeqReadBundle_1__mem.get(R1.values[0], 1);}
  val_t T4;
  { T4 = T3[1] >> 40;}
  T4 = T4 & 0xffL;
  { DelaySuite_SeqReadBundle_1__io_out_1_a_a.values[0] = T4;}
  val_t T5;
  { T5 = T3[1] >> 24;}
  T5 = T5 & 0xffffL;
  { DelaySuite_SeqReadBundle_1__io_out_1_a_b.values[0] = T5;}
  val_t T6;
  { T6 = T3[0] >> 56 | T3[1] << 8;}
  T6 = T6 & 0xffffffffL;
  { DelaySuite_SeqReadBundle_1__io_out_1_a_b_.values[0] = T6;}
  val_t T7;
  { T7 = T3[0] >> 48 | T3[1] << 16;}
  T7 = T7 & 0xffL;
  { DelaySuite_SeqReadBundle_1__io_out_0_a_a.values[0] = T7;}
  val_t T8;
  { T8 = T3[0] >> 32 | T3[1] << 32;}
  T8 = T8 & 0xffffL;
  { DelaySuite_SeqReadBundle_1__io_out_0_a_b.values[0] = T8;}
  { T0.values[0] = TERNARY_1(DelaySuite_SeqReadBundle_1__io_ren.values[0], DelaySuite_SeqReadBundle_1__io_raddr.values[0], R1.values[0]);}
  val_t T9;
  { T9 = DelaySuite_SeqReadBundle_1__io_in_0_a_b_.values[0] | DelaySuite_SeqReadBundle_1__io_in_0_a_b.values[0] << 32;}
  val_t T10;
  { T10 = T9 | DelaySuite_SeqReadBundle_1__io_in_0_a_a.values[0] << 48;}
  val_t T11;
  { T11 = DelaySuite_SeqReadBundle_1__io_in_1_a_b_.values[0] | DelaySuite_SeqReadBundle_1__io_in_1_a_b.values[0] << 32;}
  val_t T12;
  { T12 = T11 | DelaySuite_SeqReadBundle_1__io_in_1_a_a.values[0] << 48;}
  val_t T13[2];
  { T13[0] = T10 | T12 << 56; T13[1] = T12 >> 8;}
  { T2.values[0] = T13[0]; T2.values[1] = T13[1];}
  val_t T14;
  { T14 = T3[0];}
  T14 = T14 & 0xffffffffL;
  { DelaySuite_SeqReadBundle_1__io_out_0_a_b_.values[0] = T14;}
}
void DelaySuite_SeqReadBundle_1_t::clock_hi ( dat_t<1> reset ) {
  dat_t<4> R1__shadow = T0;
  { if (DelaySuite_SeqReadBundle_1__io_wen.values[0]) DelaySuite_SeqReadBundle_1__mem.put(DelaySuite_SeqReadBundle_1__io_waddr.values[0], 0, T2.values[0]); if (DelaySuite_SeqReadBundle_1__io_wen.values[0]) DelaySuite_SeqReadBundle_1__mem.put(DelaySuite_SeqReadBundle_1__io_waddr.values[0], 1, T2.values[1]);}
  R1 = T0;
}
void DelaySuite_SeqReadBundle_1_api_t::init_mapping_table (  ) {
  dat_table.clear();
  mem_table.clear();
  DelaySuite_SeqReadBundle_1_t* mod_typed = dynamic_cast<DelaySuite_SeqReadBundle_1_t*>(module);
  assert(mod_typed);
  dat_table["DelaySuite_SeqReadBundle_1.io_out_1_a_a"] = new dat_api<8>(&mod_typed->DelaySuite_SeqReadBundle_1__io_out_1_a_a, "DelaySuite_SeqReadBundle_1.io_out_1_a_a", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_out_1_a_b"] = new dat_api<16>(&mod_typed->DelaySuite_SeqReadBundle_1__io_out_1_a_b, "DelaySuite_SeqReadBundle_1.io_out_1_a_b", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_out_1_a_b_"] = new dat_api<32>(&mod_typed->DelaySuite_SeqReadBundle_1__io_out_1_a_b_, "DelaySuite_SeqReadBundle_1.io_out_1_a_b_", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_out_0_a_a"] = new dat_api<8>(&mod_typed->DelaySuite_SeqReadBundle_1__io_out_0_a_a, "DelaySuite_SeqReadBundle_1.io_out_0_a_a", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_out_0_a_b"] = new dat_api<16>(&mod_typed->DelaySuite_SeqReadBundle_1__io_out_0_a_b, "DelaySuite_SeqReadBundle_1.io_out_0_a_b", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_raddr"] = new dat_api<4>(&mod_typed->DelaySuite_SeqReadBundle_1__io_raddr, "DelaySuite_SeqReadBundle_1.io_raddr", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_ren"] = new dat_api<1>(&mod_typed->DelaySuite_SeqReadBundle_1__io_ren, "DelaySuite_SeqReadBundle_1.io_ren", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_in_0_a_b_"] = new dat_api<32>(&mod_typed->DelaySuite_SeqReadBundle_1__io_in_0_a_b_, "DelaySuite_SeqReadBundle_1.io_in_0_a_b_", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_in_0_a_b"] = new dat_api<16>(&mod_typed->DelaySuite_SeqReadBundle_1__io_in_0_a_b, "DelaySuite_SeqReadBundle_1.io_in_0_a_b", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_in_0_a_a"] = new dat_api<8>(&mod_typed->DelaySuite_SeqReadBundle_1__io_in_0_a_a, "DelaySuite_SeqReadBundle_1.io_in_0_a_a", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_in_1_a_b_"] = new dat_api<32>(&mod_typed->DelaySuite_SeqReadBundle_1__io_in_1_a_b_, "DelaySuite_SeqReadBundle_1.io_in_1_a_b_", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_in_1_a_b"] = new dat_api<16>(&mod_typed->DelaySuite_SeqReadBundle_1__io_in_1_a_b, "DelaySuite_SeqReadBundle_1.io_in_1_a_b", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_in_1_a_a"] = new dat_api<8>(&mod_typed->DelaySuite_SeqReadBundle_1__io_in_1_a_a, "DelaySuite_SeqReadBundle_1.io_in_1_a_a", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_wen"] = new dat_api<1>(&mod_typed->DelaySuite_SeqReadBundle_1__io_wen, "DelaySuite_SeqReadBundle_1.io_wen", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_waddr"] = new dat_api<4>(&mod_typed->DelaySuite_SeqReadBundle_1__io_waddr, "DelaySuite_SeqReadBundle_1.io_waddr", "");
  mem_table["DelaySuite_SeqReadBundle_1.mem"] = new mem_api<112, 16>(&mod_typed->DelaySuite_SeqReadBundle_1__mem, "DelaySuite_SeqReadBundle_1.mem", "");
  dat_table["DelaySuite_SeqReadBundle_1.io_out_0_a_b_"] = new dat_api<32>(&mod_typed->DelaySuite_SeqReadBundle_1__io_out_0_a_b_, "DelaySuite_SeqReadBundle_1.io_out_0_a_b_", "");
}
