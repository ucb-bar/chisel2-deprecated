// Header for Chisel emulator API
#ifndef __IS_EMULATOR_API__
#define __IS_EMULATOR_API__

#include "sim_api.h"
#include "emulator.h"

#include <string>
#include <sstream>
#include <map>
#include <cassert>
#include <cerrno>

/**
 * Converts an integer to a std::string without needing additional libraries
 * or C++11.
 */
static std::string itos(int in) {
  std::stringstream out;
  out << in;
  return out.str();
}

/**
 * Copy one val_t array to another.
 * nb must be the exact number of bits the val_t represents.
 */
static __attribute__((unused)) void val_cpy(val_t* dst, val_t* src, int nb) {
  for (int i=0; i<val_n_words(nb); i++) {
    dst[i] = src[i];
  }
}

/**
 * Empty a val_t array (sets to zero).
 * nb must be the exact number of bits the val_t represents.
 */
static void val_empty(val_t* dst, int nb) {
  for (int i=0; i<val_n_words(nb); i++) {
    dst[i] = 0;
  }
}

/**
 * Set a val_t array to a integer number. Obviously, the maximum integer
 * is capped by the width of a single val_t element.
 * nb must be the exact number of bits the val_t represents.
 */
static __attribute__((unused)) void val_set(val_t* dst, val_t nb, val_t num) {
  val_empty(dst, nb);
  dst[0] = num;
}

// API base class, providing common functions
class api_base {
public:
  api_base(const char* new_name, const char* new_path) :
    name(new_name),
    path(new_path)
  {}
  // returns the fully qualified name of this object (path + dot + name)
  std::string get_pathname() {
    if (*path == '\0') {
      return name;
    } else {
      return get_path() + "." + name;
    }
  }
  // returns the short name of this object
  std::string get_name() {
    return name;
  }
  // returns the path of this object (without a trailing dot)
  std::string get_path() {
    return path;
  }
protected:
  const char* name;
  const char* path;
};

// API base (non width templated) class for API accessors to dat_t
class dat_api_base : public api_base {
public:
  dat_api_base(const char* new_name, const char* new_path) :
    api_base(new_name, new_path)
  {}
  // returns the value of this wire as a string, or empty string on failure
  virtual std::string get_value() = 0;
  // sets the value of this wire from a string, returning true on success
  virtual bool set_value(std::string value) = 0;
  virtual void set_value(val_t value, size_t idx) = 0;
  // returns the bitwidth of this wire
  virtual size_t get_width() = 0;
  virtual size_t get_num_words() = 0;
};

template<int w> class dat_api : public dat_api_base {
public:
  dat_api(dat_t<w>* new_dat, const char* new_name, const char* new_path) :
    dat_api_base(new_name, new_path),
    dat_ptr(new_dat)
  { 
    size_t rem = w % 64;
    mask = (rem) ? (1L << rem) - 1 : -1L;
  }

  std::string get_value() {
    return dat_ptr->to_str();
  }

  bool set_value(std::string value) {
    return dat_from_hex<w>(value, *dat_ptr);
  }

  void set_value(val_t value, size_t idx) {
    dat_ptr->values[idx] = (idx == get_num_words() - 1) ? value & mask : value;
  }

  size_t get_width() { return w; }
  size_t get_num_words() { return dat_ptr->n_words_of(); }

protected:
  dat_t<w>* dat_ptr;

private:
  val_t mask;
};

// API base (non width/depth templated) class for API accessors to mem_t
class mem_api_base : public api_base {
public:
  mem_api_base(const char* new_name, const char* new_path) :
    api_base(new_name, new_path)
  {}
  // return the value of an element as a string, or empty string on failure
  virtual std::string get_element(std::string index) = 0;
  // sets the value of an element from a string, returning true on success
  virtual bool set_element(std::string index, std::string value) = 0;
  // returns the bitwidth of a memory element
  virtual std::string get_width() = 0;
  // returns the number of memory elements
  virtual std::string get_depth() = 0;
};

// mem_api dummy class, does nothing except for return errors
// to be used when a real mem_api object can't be found
class mem_dummy : public mem_api_base {
public:
  mem_dummy() :
    mem_api_base("error", "")
  {}
  std::string get_element(std::string index) {
    return "error";
  }

  bool set_element(std::string index, std::string value) {
    return false;
  }

  std::string get_width() {
    return "error";
  }

  std::string get_depth() {
    return "error";
  }
};

template<int w, int d> class mem_api : public mem_api_base {
public:
  mem_api(mem_t<w, d>* new_mem, const char* new_name, const char* new_path) :
    mem_api_base(new_name, new_path),
    mem_ptr(new_mem)
  {}

  string get_element(std::string index) {
    int index_int = atoi(index.c_str());
    return mem_ptr->contents[index_int].to_str();
  }

  bool set_element(std::string index, std::string value) {
    int index_int = atoi(index.c_str());
    return dat_from_hex<w>(value, mem_ptr->contents[index_int]);
  }

  std::string get_width() {
    return itos(w);
  }

  std::string get_depth() {
    return itos(d);
  }

protected:
  mem_t<w, d>* mem_ptr;
};

class emul_api_t: public sim_api_t {
public:
  emul_api_t(mod_t* m) {
    module = m; 
    is_exit = false;
  }
  bool exit() { return is_exit; }

protected:
  mod_t* module;
  sim_data_t<dat_api_base*> sim_data;

private:
  bool is_exit;
  virtual void reset() {
    module->clock_lo(LIT<1>(1));
    module->clock_hi(LIT<1>(1));
  }
  virtual void start() { }
  virtual void finish() { 
    is_exit = true; 
  }

  void get_tokens() {
    for (size_t i = 0 ; i < sim_data.inputs.size() ; i++) {
      dat_api_base* in = sim_data.inputs[i];
      for (ssize_t k = in->get_num_words() - 1 ; k >= 0 ; k--) {
        val_t value;
        std::cin >> std::hex >> value;
        in->set_value(value, k);
      }
    }
  }

  virtual void step() {
    get_tokens();
    module->dump();
    module->clock_lo(LIT<1>(0));
    module->clock_hi(LIT<1>(0));
    // TODO: should call twice to get the output for now
    module->clock_lo(LIT<1>(0));
  }
 
  virtual void update () {
    get_tokens();
    module->clock_lo(LIT<1>(0));
  }

  virtual void gen_tokens() {
    // module->clock(is_reset);
    for (size_t i = 0 ; i < sim_data.outputs.size() ; i++) {
      std::cout << sim_data.outputs[i]->get_value() << std::endl;
    }
  }
};

#endif
