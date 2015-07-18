#ifndef __VPI_H
#define __VPI_h

#include "vpi_user.h"
#include "sim_api.h"

class vpi_api_t: public sim_api_t {
public:
  void init_rsts() {
    vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
    // Cache Resets
    while (vpiHandle arg_handle = vpi_scan(arg_iter)) {
      sim_data.resets.push_back(arg_handle);
    }
  }

  void init_ins() {
    vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
    // Cache Inputs  
    while (vpiHandle arg_handle = vpi_scan(arg_iter)) {
      sim_data.inputs.push_back(arg_handle);
    }
  }

  void init_outs() {
    vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
    // Cache Outputs
    while (vpiHandle arg_handle = vpi_scan(arg_iter)) {
      sim_data.outputs.push_back(arg_handle);
    }
  }

private:
  sim_data_t<vpiHandle> sim_data;  

  virtual void reset() {
    for (size_t i = 0 ; i < sim_data.resets.size() ; i++) {
      s_vpi_value value_s;
      value_s.format = vpiHexStrVal;
      value_s.value.str = (PLI_BYTE8*) "1";
      vpi_put_value(sim_data.resets[i], &value_s, NULL, vpiNoDelay);
    }
  }

  virtual void start() {
    for (size_t i = 0 ; i < sim_data.resets.size() ; i++) {
      s_vpi_value value_s;
      value_s.format = vpiHexStrVal;
      value_s.value.str = (PLI_BYTE8*) "0";
      vpi_put_value(sim_data.resets[i], &value_s, NULL, vpiNoDelay);
    }
  }

  virtual void finish() {
    vpi_control(vpiFinish, 0);
  }

  virtual void consume_inputs() {
    // Consumes input tokens (in hex)
    for (size_t i = 0 ; i < sim_data.inputs.size() ; i++) {
      std::string value;
      std::cin >> value;
      s_vpi_value value_s;
      value_s.format = vpiHexStrVal;
      value_s.value.str = (PLI_BYTE8*) value.c_str();
      vpi_put_value(sim_data.inputs[i], &value_s, NULL, vpiNoDelay);
    }
  }

  virtual void generate_outputs() {
    // Generate output tokens (in hex)
    for (size_t i = 0 ; i < sim_data.outputs.size() ; i++) {
      s_vpi_value value_s;
      value_s.format = vpiHexStrVal;
      vpi_get_value(sim_data.outputs[i], &value_s);
      std::cout << value_s.value.str << std::endl;
    }
  }
};

#endif // __VPI_H
