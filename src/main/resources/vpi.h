#ifndef __VPI_H
#define __VPI_h

#include "vpi_user.h"
#include "sim_api.h"
#include <queue>

PLI_INT32 update_cb(p_cb_data cb_data);

class vpi_api_t: public sim_api_t<vpiHandle> {
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

  void init_sigs() {
    vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
    // argument: filename
    s_vpi_value file_s;
    file_s.format = vpiStringVal;
    vpi_get_value(vpi_scan(arg_iter), &file_s);
    read_signal_map(file_s.value.str);

    // Now, serach signals
    vpiHandle test_handle = vpi_scan(vpi_iterate(vpiModule, NULL));
    vpiHandle top_handle = vpi_scan(vpi_iterate(vpiModule, test_handle));
    std::string testname = vpi_get_str(vpiDefName, test_handle);
    size_t offset = testname.length() + 1;
    std::queue<vpiHandle> modules;
    std::map<int, vpiHandle> handles;
    modules.push(top_handle);
    while (!modules.empty()) {
      vpiHandle mod_handle = modules.front();
      modules.pop();
      // Iterate its net
      vpiHandle net_iter = vpi_iterate(vpiNet, mod_handle);
      while (vpiHandle net_handle = vpi_scan(net_iter)) {
        std::string nodepath = std::string(vpi_get_str(vpiFullName, net_handle)).substr(offset);
        if (signals.find(nodepath) != signals.end()) {
          handles[signals[nodepath]] = net_handle;
        }
      }

      // Iterate its reg
      vpiHandle reg_iter = vpi_iterate(vpiReg, mod_handle);
      while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
        std::string nodepath = std::string(vpi_get_str(vpiFullName, reg_handle)).substr(offset);
        if (signals.find(nodepath) != signals.end()) {
          handles[signals[nodepath]] = reg_handle;
        }
      }

      // Iterate its mem
      vpiHandle mem_iter = vpi_iterate(vpiRegArray, mod_handle);
      while (vpiHandle mem_handle = vpi_scan(mem_iter)) {
        std::string nodepath = std::string(vpi_get_str(vpiFullName, mem_handle)).substr(offset);
        if (signals.find(nodepath) != signals.end()) {
          size_t id = signals[nodepath] + vpi_get(vpiSize, mem_handle);
          vpiHandle elm_iter = vpi_iterate(vpiReg, mem_handle);
          while (vpiHandle elm_handle = vpi_scan(elm_iter)) {
            handles[--id] = elm_handle;
          }
        }
      }

      vpiHandle sub_iter = vpi_iterate(vpiModule, mod_handle);
      while (vpiHandle sub_handle = vpi_scan(sub_iter)) {
        modules.push(sub_handle);
      }
    }

    for (size_t i = 0 ; i < handles.size() ; i++) {
      sim_data.signals.push_back(handles[i]);
    }
  }

private:
  void put_value(vpiHandle& sig) {
    std::string value;
    for (size_t k = 0 ; k < ((vpi_get(vpiSize, sig) - 1) >> 6) + 1 ; k++) {
      // 64 bit chunks are given
      std::string v;
      std::cin >> v;
      value += v;
    }
    s_vpi_value value_s;
    value_s.format = vpiHexStrVal;
    value_s.value.str = (PLI_BYTE8*) value.c_str();
    vpi_put_value(sig, &value_s, NULL, vpiNoDelay);
  }

  void get_value(vpiHandle& sig) {
    s_vpi_value value_s;
    value_s.format = vpiHexStrVal;
    vpi_get_value(sig, &value_s);
    std::cout << value_s.value.str << std::endl;
  }

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

  virtual void finish() { vpi_control(vpiFinish, 0); }

  virtual void step() { }

  virtual void update() {
    s_cb_data data_s;
    s_vpi_time time_s;
    time_s.type      = vpiSimTime;
    time_s.low       = 0;
    time_s.high      = 0;
    data_s.reason    = cbReadWriteSynch;
    data_s.cb_rtn    = update_cb;
    data_s.obj       = NULL;
    data_s.time      = &time_s;
    data_s.value     = NULL;
    data_s.user_data = NULL;
    vpi_free_object(vpi_register_cb(&data_s));
  }
};

#endif // __VPI_H
