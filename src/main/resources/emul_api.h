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
    for (ssize_t k = sig->get_num_words() - 1 ; k >= 0 ; k--) {
      val_t value;
      std::cin >> std::hex >> value;
      sig->put_value(value, k);
    }
  }

  virtual void get_value(dat_api_base* &sig) {
    std::cout << sig->get_value() << std::endl;
  }

  bool is_exit;
  virtual void reset() {
    module->clock_lo(LIT<1>(1));
    module->clock_hi(LIT<1>(1));
    // TODO: should call twice to get the output for now
    module->clock_lo(LIT<1>(0));
  }

  virtual void start() { }

  virtual void finish() { 
    is_exit = true; 
  }

  virtual void step() {
    module->dump();
    module->print(std::cerr);
    module->clock_lo(LIT<1>(0));
    module->clock_hi(LIT<1>(0));
    // TODO: should call twice to get the output for now
    module->clock_lo(LIT<1>(0));
  }
 
  virtual void update() {
    module->clock_lo(LIT<1>(0));
  }
};

#endif
