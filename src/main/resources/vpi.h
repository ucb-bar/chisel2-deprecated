#ifndef __VPI_H
#define __VPI_h

#include "vpi_user.h"
#include "sim_api.h"
#include <queue>

PLI_INT32 tick_cb(p_cb_data cb_data);

class vpi_api_t: public sim_api_t<vpiHandle> {
public:
  void init_tick() {
    vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle arg_iter(vpi_iterate(vpiArgument, syscall_handle));

    // Argument: clock delay
    s_cb_data   data_s;
    s_vpi_time  time_s;
    s_vpi_value value_s;
    value_s.format = vpiRealVal;
    vpi_get_value(vpi_scan(arg_iter), &value_s);
    time_s.type      = vpiScaledRealTime;
    time_s.real      = value_s.value.real;
    data_s.reason    = cbAfterDelay;
    data_s.cb_rtn    = tick_cb;
    data_s.obj       = syscall_handle; 
    data_s.time      = &time_s;
    data_s.value     = NULL;
    data_s.user_data = NULL;
    vpi_free_object(vpi_register_cb(&data_s));
  }

  void init_top() {
    vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
    top_handle = vpi_scan(vpi_iterate(vpiArgument, syscall_handle));
  }

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

    // Argument: filename
    s_vpi_value file_s;
    file_s.format = vpiStringVal;
    vpi_get_value(vpi_scan(arg_iter), &file_s);

    // First read signal map
    read_signal_map(file_s.value.str);

    // Now, serach signals
    std::queue<vpiHandle> modules;
    std::map<int, vpiHandle> handles;
    size_t offset = std::string(vpi_get_str(vpiFullName, top_handle)).find(".") + 1;
    modules.push(top_handle);
    while (!modules.empty()) {
      vpiHandle mod_handle = modules.front();
      modules.pop();
      // Iterate its net
      vpiHandle net_iter = vpi_iterate(vpiNet, mod_handle);
      while (vpiHandle net_handle = vpi_scan(net_iter)) {
        std::string nodepath = std::string(vpi_get_str(vpiFullName, net_handle)).substr(offset);
        std::map<std::string, size_t>::iterator s = sim_data.signal_map.find(nodepath);
        if (s != sim_data.signal_map.end()) handles[s->second] = net_handle;
      }
      
      // Iterate its reg
      vpiHandle reg_iter = vpi_iterate(vpiReg, mod_handle);
      while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
        std::string nodepath = std::string(vpi_get_str(vpiFullName, reg_handle)).substr(offset);
        std::map<std::string, size_t>::iterator s = sim_data.signal_map.find(nodepath);
        if (s != sim_data.signal_map.end()) handles[s->second] = reg_handle;
      }

      // Iterate its mem
      vpiHandle mem_iter = vpi_iterate(vpiRegArray, mod_handle);
      while (vpiHandle mem_handle = vpi_scan(mem_iter)) {
        std::string nodepath = std::string(vpi_get_str(vpiFullName, mem_handle)).substr(offset);
        std::map<std::string, size_t>::iterator s = sim_data.signal_map.find(nodepath);
        if (s != sim_data.signal_map.end()) {
          size_t id = s->second + vpi_get(vpiSize, mem_handle);
          vpiHandle elm_iter = vpi_iterate(vpiReg, mem_handle);
          while (vpiHandle elm_handle = vpi_scan(elm_iter)) handles[--id] = elm_handle;
        }
      }

      // Iterate vec(?)
      vpiHandle vec_iter = vpi_iterate(vpiNetArray, mod_handle);
      while (vpiHandle vec_handle = vpi_scan(vec_iter)) {
        std::string nodepath = std::string(vpi_get_str(vpiFullName, vec_handle)).substr(offset);
        std::map<std::string, size_t>::iterator s = sim_data.signal_map.find(nodepath);
        if (s != sim_data.signal_map.end()) {
          size_t id = s->second + vpi_get(vpiSize, vec_handle);
          vpiHandle elm_iter = vpi_iterate(vpiNet, vec_handle);
          while (vpiHandle elm_handle = vpi_scan(elm_iter)) handles[--id] = elm_handle;
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
  vpiHandle top_handle;

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
    std::cerr << value_s.value.str << std::endl;
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
    data_s.cb_rtn    = tick_cb;
    data_s.obj       = NULL;
    data_s.time      = &time_s;
    data_s.value     = NULL;
    data_s.user_data = NULL;
    vpi_free_object(vpi_register_cb(&data_s));
  }

  virtual size_t add_signal(vpiHandle& sig_handle, std::string& wire) {
    size_t id = sim_data.signals.size();
    sim_data.signals.push_back(sig_handle);
    sim_data.signal_map[wire] = id;
    return id;
  }

  virtual int search(std::string& wire) {
    int id = -1;
    // otherwise, search it
    int dotpos = wire.rfind(".");
    int sbrpos = wire.rfind("[");
    std::string modpath = wire.substr(0, dotpos);
    std::string wirename = wire.substr(dotpos+1);
    std::string memname = sbrpos > 0 ? wire.substr(sbrpos) : "";
    std::queue<vpiHandle> modules;
    size_t offset = std::string(vpi_get_str(vpiFullName, top_handle)).find(".") + 1;

    // Start from the top module
    modules.push(top_handle);

    while (!modules.empty()) {
      vpiHandle mod_handle = modules.front();
      modules.pop();

      // If the module is found
      if (modpath == std::string(vpi_get_str(vpiFullName, mod_handle)).substr(offset)) {
        // Iterate its nets
        vpiHandle net_iter = vpi_iterate(vpiNet, mod_handle);
        while (vpiHandle net_handle = vpi_scan(net_iter)) {
          if (wirename == vpi_get_str(vpiName, net_handle)) {
            id = add_signal(net_handle, wire); break;
          }
        }
        if (id > 0) break;

        // Iterate its regs
        vpiHandle reg_iter = vpi_iterate(vpiReg, mod_handle);
        while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
          if (wirename == vpi_get_str(vpiName, reg_handle)) {
            id = add_signal(reg_handle, wire); break;
          }
        }
        if (id > 0) break;

        // Iterate its mems
        vpiHandle mem_iter = vpi_iterate(vpiRegArray, mod_handle);
        while (vpiHandle mem_handle = vpi_scan(mem_iter)) {
          if (memname == vpi_get_str(vpiName, mem_handle)) {
            vpiHandle elm_iter = vpi_iterate(vpiReg, mem_handle);
            while (vpiHandle elm_handle = vpi_iterate(vpiReg, mem_handle)) {
              std::string elmname = vpi_get_str(vpiName, elm_handle);
              size_t i = add_signal(elm_handle, elmname);
              if (wirename == elmname) id = i;
            }
            break;
          }
        }

        // Iterate its vec(?)
        vpiHandle vec_iter = vpi_iterate(vpiNetArray, mod_handle);
        while (vpiHandle vec_handle = vpi_scan(vec_iter)) {
          if (memname == vpi_get_str(vpiName, vec_handle)) {
            vpiHandle elm_iter = vpi_iterate(vpiNet, vec_handle);
            while (vpiHandle elm_handle = vpi_iterate(vpiNet, vec_handle)) {
              std::string elmname = vpi_get_str(vpiName, elm_handle);
              size_t i = add_signal(elm_handle, elmname);
              if (wirename == elmname) id = i;
            }
            break;
          }
        }
      }
      vpiHandle sub_iter = vpi_iterate(vpiModule, mod_handle);
      while (vpiHandle sub_handle = vpi_scan(sub_iter)) {
        modules.push(sub_handle);
      }
    }
    return id;
  }
};

#endif // __VPI_H
