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
    mask = (rem) ? (1L << rem) - 1 : -1L;
  }
  virtual std::string get_value() = 0;
  virtual bool put_value(std::string value) = 0;
  virtual void put_value(val_t value, size_t idx) = 0;
  virtual size_t get_width() = 0;
  virtual size_t get_num_words() = 0;
protected:
  val_t mask;
};

template<int w> class dat_api : public dat_api_base {
public:
  dat_api(dat_t<w>* new_dat): dat_api_base(w), dat_ptr(new_dat) { }
  std::string get_value() { 
    return dat_ptr->to_str(); 
  }
  bool put_value(std::string value) { 
    return dat_from_hex<w>(value, *dat_ptr); 
  }
  void put_value(val_t value, size_t idx) {
    dat_ptr->values[idx] = (idx == get_num_words() - 1) ? value & mask : value;
  }
  size_t get_width() { return w; }
  size_t get_num_words() { return dat_ptr->n_words_of(); }

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
  std::string get_value() { return itos(clk_ptr->len); }
  bool put_value(std::string value) { return false; }
  void put_value(val_t value, size_t idx /* not used */) {
    clk_ptr->len = (size_t) value;
    clk_ptr->cnt = (size_t) value;
  }
  size_t get_width() { return 8*sizeof(size_t); }
  size_t get_num_words() { return 1; }

private:
  clk_t* clk_ptr;
};

class emul_api_t: public sim_api_t<dat_api_base*> {
public:
  emul_api_t(mod_t* m) {
    module = m; 
    is_exit = false;
  }
  bool exit() { return is_exit; }

protected:
  mod_t* module;

private:
  virtual void put_value(dat_api_base* &sig) {
    std::string value;
    std::cin >> value;
    sig->put_value(value);
  }

  virtual void get_value(dat_api_base* &sig) {
    std::cerr << sig->get_value() << std::endl;
  }

  bool is_exit;
  virtual void reset() {
    module->clock(LIT<1>(1));
    // TODO: should call twice to get the output for now
    module->clock_lo(LIT<1>(0));
  }

  virtual void start() { }

  virtual void finish() { 
    module->dump();
    is_exit = true; 
  }

  virtual void step() {
    module->dump();
    module->print(std::cout);
    module->clock(LIT<1>(0));
    // TODO: should call twice to get the output for now
    module->clock_lo(LIT<1>(0));
  }
 
  virtual void update() {
    module->clock_lo(LIT<1>(0));
  }
};

#endif
