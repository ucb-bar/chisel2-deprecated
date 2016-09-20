// Header for Chisel emulator API
#ifndef __IS_EMULATOR_API__
#define __IS_EMULATOR_API__

#include "sim_api.h"
#include "emulator.h"

// API base (non width templated) class for API accessors to dat_t
class dat_api_base { 
public:
  dat_api_base(size_t w) {
    size_t rem = w % 64;
    mask = rem ? (1L << rem) - 1 : -1L;
  }
  virtual std::string get_value() = 0;
  virtual bool put_value(std::string &value) = 0;
  virtual size_t get_value(val_t* values) = 0;
  virtual size_t put_value(val_t* values) = 0;
  virtual size_t get_width() = 0;
  virtual size_t get_num_words() = 0;
protected:
  val_t mask;
};

template<int w> class dat_api : public dat_api_base {
public:
  dat_api(dat_t<w>* new_dat): dat_api_base(w), dat_ptr(new_dat) { }
  inline std::string get_value() { 
    return dat_ptr->to_str(); 
  }
  inline bool put_value(std::string &value) { 
    return dat_from_hex<w>(value, *dat_ptr); 
  }
  inline size_t get_value(val_t* values) {
    size_t i = 0;
    for ( ; i < get_num_words() ; i++) {
      val_t value = dat_ptr->values[i];
      values[i] = (i == (get_num_words()-1)) ? value & mask : value;
    }
    return i;
  }
  inline size_t put_value(val_t* values) {
    size_t i = 0;
    for ( ; i < get_num_words() ; i++) {
      val_t value = values[i];
      dat_ptr->values[i] = (i == (get_num_words()-1)) ? value & mask : value;
    }
    return i; 
  }
  inline size_t get_width() { return w; }
  inline size_t get_num_words() { return dat_ptr->n_words_of(); }

private:
  dat_t<w>* dat_ptr;
};

inline std::string itos(int in, bool is_hex = true) {
  std::stringstream out;
  if (is_hex) out << std::hex;
  out << in;
  return out.str();
}

class clk_api: public dat_api_base {
public:
  clk_api(clk_t* new_clk): dat_api_base(1), clk_ptr(new_clk) { }
  inline std::string get_value() { return itos(clk_ptr->len); }
  inline bool put_value(std::string &value) { return false; }
  inline size_t get_value(val_t* values) {
   values[0] = (val_t) clk_ptr->len; 
   return 1; 
  }
  inline size_t put_value(val_t* values) { 
    clk_ptr->len = (size_t) values[0];
    clk_ptr->cnt = (size_t) values[0];
    return 1; 
  }
  inline size_t get_width() { return 8*sizeof(size_t); }
  inline size_t get_num_words() { return 1; }

private:
  clk_t* clk_ptr;
};

class emul_api_t: public sim_api_t<dat_api_base*> {
public:
  emul_api_t(mod_t* m) {
    module = m; 
    is_exit = false;
  }
  inline bool exit() { return is_exit; }

protected:
  mod_t* module;

private:
  virtual inline void put_value(dat_api_base* &sig, std::string& value, bool force=false) {
    sig->put_value(value);
  }

  virtual inline size_t put_value(dat_api_base* &sig, uint64_t* data, bool force=false) {
    return sig->put_value(data);
  }

  virtual inline std::string get_value(dat_api_base* &sig) {
    return sig->get_value();
  }
  
  virtual inline size_t get_value(dat_api_base* &sig, uint64_t* data) {
    return sig->get_value(data);
  }

  virtual inline size_t get_chunk(dat_api_base* &sig) {
    return sig->get_num_words();
  } 

  virtual inline void reset() {
    module->clock(LIT<1>(1));
    // FIXME: should call twice to get the output for now
    module->clock_lo(LIT<1>(0), false);
  }

  virtual inline void start() { }

  bool is_exit;
  virtual inline void finish() {
    module->clock(LIT<1>(0)); // to vcd-dump the last cycle
    is_exit = true; 
  }

  virtual inline void step() {
    module->clock(LIT<1>(0));
    // FIXME: should call twice to get the output for now
    module->clock_lo(LIT<1>(0), false);
  }
 
  virtual inline void update() {
    module->clock_lo(LIT<1>(0), false);
  }
};

#endif
