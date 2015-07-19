#ifndef __SIM_API_H
#define __SIM_API_H

#include <iostream>
#include <vector>

enum SIM_CMD { RESET, STEP, UPDATE, FIN };

template<class T> 
struct sim_data_t {
  std::vector<T> resets;
  std::vector<T> inputs;
  std::vector<T> outputs;
};

class sim_api_t {
public:
  void tick() {
    static bool is_reset = false;
    // First, Generates output tokens  (in hex)
    gen_tokens();
    if (is_reset) {
      start();
      is_reset = false;
    }
    
    // Next, handle commands from the testers
    bool exit = false;
    do {
      size_t cmd;
      std::cin >> cmd;
      switch ((SIM_CMD) cmd) {
        case RESET: reset(); is_reset = true; exit = true; break;
        case STEP: step(); exit = true; break;
        case UPDATE: update(); exit = true; break;
        case FIN:  finish(); exit = true; break;
        default: break;
      }
    } while (!exit);
  }
private:
  virtual void reset() = 0;
  virtual void start() = 0; 
  virtual void finish() = 0;
  virtual void update() = 0; 
  virtual void step() = 0;
  virtual void gen_tokens() = 0;
};

#endif //__SIM_API_H
