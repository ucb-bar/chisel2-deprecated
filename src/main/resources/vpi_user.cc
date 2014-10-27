#include <fstream>
#include <sstream>
#include <string>
#include <set>
#include <map>
#include <queue>
#include <ctime>
#include <vpi_user.h>

using namespace std;

/*==========================================================================
                 User Functions
=============================================================================*/
int32_t wire_poke_calltf(char *user_data) {
  queue<vpiHandle> modules;
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
  // First argument: <node_name>
  s_vpi_value node_s;
  node_s.format = vpiStringVal;
  vpi_get_value(vpi_scan(arg_iter), &node_s);
  // Second argument: <value>
  s_vpi_value value_s;
  value_s.format = vpiIntVal;
  vpi_get_value(vpi_scan(arg_iter), &value_s);
  vpi_free_object(arg_iter);

  vpiHandle test_handle = vpi_scan(vpi_iterate(vpiModule, NULL));
  vpiHandle top_handle = vpi_scan(vpi_iterate(vpiModule, test_handle));
  // Construct node paths
  string testname = vpi_get_str(vpiDefName, test_handle);
  istringstream iss(node_s.value.str);
  string nodename;
  iss >> nodename;
  ostringstream oss;
  oss << testname << "." << nodename;
  string nodepath = oss.str();

  // Examine the regs in the testbench
  // in order to give the correct input values
  string modulepath = vpi_get_str(vpiFullName, top_handle);
  string testpath = testname + nodepath.substr(modulepath.length(), nodepath.length() - modulepath.length());
  vpiHandle reg_iter = vpi_iterate(vpiReg, test_handle);
  while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
    if (testpath == vpi_get_str(vpiFullName, reg_handle)) {
      vpi_put_value(reg_handle, &value_s, NULL, vpiNoDelay);
      vpi_printf("ok\n");
      return 0;
    }
  }

  // Start from the top module
  modules.push(top_handle);

  bool found = false;
  while (!modules.empty()) {
    vpiHandle mod_handle = modules.front();
    modules.pop();

    // Iterate its net
    vpiHandle net_iter = vpi_iterate(vpiNet, mod_handle);
    while (vpiHandle net_handle = vpi_scan(net_iter)) {
      if (nodepath == vpi_get_str(vpiFullName, net_handle)) {
        vpi_put_value(net_handle, &value_s, NULL, vpiNoDelay);
        found = true;
      }
      if (found) break;
    }
    if (found) break;

    // Iterate its reg
    vpiHandle reg_iter = vpi_iterate(vpiReg, mod_handle);
    while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
      if (nodepath == vpi_get_str(vpiFullName, reg_handle)) {
        vpi_put_value(reg_handle, &value_s, NULL, vpiNoDelay);
        found = true;
      }
      if (found) break;
    }
    if (found) break;

    vpiHandle sub_iter = vpi_iterate(vpiModule, mod_handle);
    while (vpiHandle sub_handle = vpi_scan(sub_iter)) {
      modules.push(mod_handle);
    }
  }

  if (found)
    vpi_printf("ok\n");
  else
    vpi_printf("error\n");

  return 0;
}

int32_t wire_peek_calltf(char *user_data) {
  queue<vpiHandle> modules;
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
  // First argument: <node_name>
  s_vpi_value node_s;
  node_s.format = vpiStringVal;
  vpi_get_value(vpi_scan(arg_iter), &node_s);
  vpi_free_object(arg_iter);

  vpiHandle test_handle = vpi_scan(vpi_iterate(vpiModule, NULL));
  vpiHandle top_handle = vpi_scan(vpi_iterate(vpiModule, test_handle));
  // Construct node paths
  string testname = vpi_get_str(vpiDefName, test_handle);
  istringstream iss(node_s.value.str);
  string nodename;
  iss >> nodename;
  ostringstream oss;
  oss << testname << "." << nodename;
  string nodepath = oss.str();
  // Start from the top module
  modules.push(top_handle);

  s_vpi_value value_s;
  value_s.format = vpiHexStrVal;
  bool found = false;
  while (!modules.empty()) {
    vpiHandle mod_handle = modules.front();
    modules.pop();

    // Iterate its net
    vpiHandle net_iter = vpi_iterate(vpiNet, mod_handle);
    while (vpiHandle net_handle = vpi_scan(net_iter)) {
      if (nodepath == vpi_get_str(vpiFullName, net_handle)) {
        vpi_get_value(net_handle, &value_s);
        found = true;
      }
    }
    if (found) break;

    // Iterate its reg
    vpiHandle reg_iter = vpi_iterate(vpiReg, mod_handle);
    while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
      if (nodepath == vpi_get_str(vpiFullName, reg_handle)) {
        vpi_get_value(reg_handle, &value_s);
        found = true;
      }
    }
    if (found) break;

    vpiHandle sub_iter = vpi_iterate(vpiModule, mod_handle);
    while (vpiHandle sub_handle = vpi_scan(sub_iter)) {
      modules.push(mod_handle);
    }
  }

  if (found)
    vpi_printf("0x%s\n", value_s.value.str);
  else
    vpi_printf("error\n");

  return 0;
}

int32_t mem_poke_calltf(char *user_data) {
  queue<vpiHandle> modules;
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
  // First argument: <mem_name>
  s_vpi_value node_s;
  node_s.format = vpiStringVal;
  vpi_get_value(vpi_scan(arg_iter), &node_s);
  // Second argument: <mem_index>
  s_vpi_value index_s;
  index_s.format = vpiIntVal;
  vpi_get_value(vpi_scan(arg_iter), &index_s);
  // Third argument: <value>
  s_vpi_value value_s;
  value_s.format = vpiIntVal;
  vpi_get_value(vpi_scan(arg_iter), &value_s);

  vpiHandle test_handle = vpi_scan(vpi_iterate(vpiModule, NULL));
  vpiHandle top_handle = vpi_scan(vpi_iterate(vpiModule, test_handle));
  // Construct node paths
  string testname = vpi_get_str(vpiDefName, test_handle);
  istringstream iss(node_s.value.str);
  string nodename;
  iss >> nodename;
  ostringstream oss;
  oss << testname << "." << nodename;
  string nodepath = oss.str();
  oss << "[" << index_s.value.integer << "]";
  string elmpath = oss.str();
  
  // Examine the reg arrays in the testbench
  // in order to give the correct input values
  string modulepath = vpi_get_str(vpiFullName, top_handle);
  string testpath = testname + nodepath.substr(modulepath.length(), nodepath.length() - modulepath.length());
  vpiHandle reg_iter = vpi_iterate(vpiRegArray, test_handle);
  while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
    if (testpath == vpi_get_str(vpiFullName, reg_handle)) {
      vpiHandle elm_iter = vpi_iterate(vpiReg, reg_handle);
      while (vpiHandle elm_handle = vpi_scan(elm_iter)) {
          if (elmpath == vpi_get_str(vpiFullName, elm_handle)) {
          vpi_put_value(elm_handle, &value_s, NULL, vpiNoDelay);
          vpi_printf("ok\n");
          return 0;
        }
      }
    }
  }

  // Start from the top module
  modules.push(top_handle);

  bool found = false;
  while (!modules.empty()) {
    vpiHandle mod_handle = modules.front();
    modules.pop();

    // Iterate its net arrays
    vpiHandle net_iter = vpi_iterate(vpiNetArray, mod_handle);
    while (vpiHandle net_handle = vpi_scan(net_iter)) {
      if (nodepath == vpi_get_str(vpiFullName, net_handle)) {
        vpiHandle elm_iter = vpi_iterate(vpiNet, net_handle);
        while (vpiHandle elm_handle = vpi_scan(elm_iter)) {
          if (elmpath == vpi_get_str(vpiFullName, elm_handle)){
            vpi_put_value(elm_handle, &value_s, NULL, vpiNoDelay);
            found = true;
          }
          if (found) break;
        }
      }
      if (found) break;
    }
    if (found) break;

    // Iterate its reg arrays
    vpiHandle reg_iter = vpi_iterate(vpiRegArray, mod_handle);
    while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
      if (nodepath == vpi_get_str(vpiFullName, reg_handle)) {
        vpiHandle elm_iter = vpi_iterate(vpiReg, reg_handle);
        while (vpiHandle elm_handle = vpi_scan(elm_iter)) {
          if (elmpath == vpi_get_str(vpiFullName, elm_handle)){
            vpi_put_value(elm_handle, &value_s, NULL, vpiNoDelay);
            found = true;
          }
          if (found) break;
        }
      }
      if (found) break;
    }
    if (found) break;

    vpiHandle sub_iter = vpi_iterate(vpiModule, mod_handle);
    while (vpiHandle sub_handle = vpi_scan(sub_iter)) {
      modules.push(mod_handle);
    }
  }

  if (found)
    vpi_printf("ok\n");
  else
    vpi_printf("error\n");
}

int32_t mem_peek_calltf(char *user_data) {
  queue<vpiHandle> modules;
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
  // First argument: <node_name>
  s_vpi_value node_s;
  node_s.format = vpiStringVal;
  vpi_get_value(vpi_scan(arg_iter), &node_s);
  // Second argument: <mem_index>
  s_vpi_value index_s;
  index_s.format = vpiIntVal;
  vpi_get_value(vpi_scan(arg_iter), &index_s);

  vpiHandle test_handle = vpi_scan(vpi_iterate(vpiModule, NULL));
  vpiHandle top_handle = vpi_scan(vpi_iterate(vpiModule, test_handle));
  // Construct node paths
  string testname = vpi_get_str(vpiDefName, test_handle);
  istringstream iss(node_s.value.str);
  string nodename;
  iss >> nodename;
  ostringstream oss;
  oss << testname << "." << nodename;
  string nodepath = oss.str();
  oss << "[" << index_s.value.integer << "]";
  string elmpath = oss.str();
  // Start from the top module
  modules.push(top_handle);

  s_vpi_value value_s;
  value_s.format = vpiHexStrVal;
  bool found = false;
  while (!modules.empty()) {
    vpiHandle mod_handle = modules.front();
    modules.pop();

    // Iterate its net arrays
    vpiHandle net_iter = vpi_iterate(vpiNetArray, mod_handle);
    while (vpiHandle net_handle = vpi_scan(net_iter)) {
      if (nodepath == vpi_get_str(vpiFullName, net_handle)) {
        vpiHandle elm_iter = vpi_iterate(vpiNet, net_handle);
        while (vpiHandle elm_handle = vpi_scan(elm_iter)) {
          if (elmpath == vpi_get_str(vpiFullName, elm_handle)){
            vpi_get_value(elm_handle, &value_s);
            found = true;
          }
          if (found) break;
        }
      }
      if (found) break;
    }
    if (found) break;

    // Iterate its reg arrays
    vpiHandle reg_iter = vpi_iterate(vpiRegArray, mod_handle);
    while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
      if (nodepath == vpi_get_str(vpiFullName, reg_handle)) {
        vpiHandle elm_iter = vpi_iterate(vpiReg, reg_handle);
        while (vpiHandle elm_handle = vpi_scan(elm_iter)) {
          if (elmpath == vpi_get_str(vpiFullName, elm_handle)){
            vpi_get_value(elm_handle, &value_s);
            found = true;
          }
          if (found) break;
        }
      }
      if (found) break;
    }
    if (found) break;

    vpiHandle sub_iter = vpi_iterate(vpiModule, mod_handle);
    while (vpiHandle sub_handle = vpi_scan(sub_iter)) {
      modules.push(mod_handle);
    }
  }

  if (found)
    vpi_printf("0x%s\n", value_s.value.str);
  else
    vpi_printf("error\n");
}

/*
void read_file(string filename, set<string> &regpaths, map<string, string> &regvalues) {
  ifstream in(filename);
  string line;
  while (getline(in, line)) {
    istringstream iss(line);
    string force, deposit, netpath, regpath;
    char value;
    if (iss >> force >> deposit >> netpath >> value) {
      regpath = netpath.substr(0, netpath.rfind(".")); 
      regpaths.insert(regpath);
      regvalues[netpath] = value;
    }
  }
  return;
}

void encode_vpi_vecval (p_vpi_vecval vecval, string value, int offset = 0) {
  if (value == "1") {
    vecval->aval |= 1 << offset;
  } else if (value == "z") {
    vecval->bval |= 1 << offset;
  } else if (value == "x") {
    vecval->aval |= 1 << offset;
    vecval->bval |= 1 << offset;
  }
  return;
}

int32_t force_regs_calltf(char *user_data) {
  set<string> regpaths;
  map<string, string> regvalues;
  queue<vpiHandle> mod_iters;

  vpi_printf("Start initialization\n");
  clock_t begin_clock = clock();
  // Read the top module passed as an argument
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
  // First argument: force_regs file name
  s_vpi_value file_s;
  file_s.format = vpiStringVal;
  vpi_get_value(vpi_scan(arg_iter), &file_s);
  istringstream iss(file_s.value.str);
  string filename;
  iss >> filename;

  // read force_regs
  read_file(filename, regpaths, regvalues);

  // Second argument: the Top component
  mod_iters.push(vpi_iterate(vpiModule, vpi_scan(arg_iter)));
  vpi_free_object(arg_iter);

  while (!mod_iters.empty()) {
    vpiHandle mod_iter = mod_iters.front();
    mod_iters.pop();
    // Iterate submodules
    while (vpiHandle mod_handle = vpi_scan(mod_iter)) {
      // Look for flip-flops
      if (regpaths.find(vpi_get_str(vpiFullName, mod_handle)) != regpaths.end()) {
        // Iterate its net -> Find flip-flops' output port Q
        vpiHandle net_iter = vpi_iterate(vpiNet, mod_handle);
        while (vpiHandle net_handle = vpi_scan(net_iter)) {
          string fullpath = vpi_get_str(vpiFullName, net_handle);
          if (regvalues.find(fullpath) != regvalues.end()) {
            s_vpi_value net_value;
            net_value.format = vpiVectorVal;
            net_value.value.vector = new s_vpi_vecval;
            net_value.value.vector->aval = 0;
            net_value.value.vector->bval = 0;
            encode_vpi_vecval(net_value.value.vector, regvalues[fullpath]);
            vpi_put_value(net_handle, &net_value, NULL, vpiNoDelay);
          }
        }
        // Iterate its reg -> Find SRAMs' output port
        vpiHandle reg_iter = vpi_iterate(vpiReg, mod_handle);
        while (vpiHandle reg_handle = vpi_scan(reg_iter)) {
          string srampath = vpi_get_str(vpiFullName, reg_handle);
          string findpath = srampath + "\\[0\\]";
          if (regvalues.find(findpath) != regvalues.end()) {
            uint8_t reg_size = vpi_get(vpiSize, reg_handle);
            uint8_t arr_size = (reg_size - 1) / 32 + 1;
            s_vpi_value reg_value;
            reg_value.format = vpiVectorVal;
            reg_value.value.vector = new s_vpi_vecval[arr_size];
            for (int i = 0 ; i < arr_size ; i++) {
              reg_value.value.vector[i].aval = 0;
              reg_value.value.vector[i].bval = 0;
              for (int j = 0 ; j < 32 ; j++) {
                ostringstream fullpath;
                fullpath << srampath << "\\[" << (i * 32 + j) << "\\]";
                encode_vpi_vecval(&reg_value.value.vector[i], regvalues[fullpath.str()], j);
                if (i * 32 + j >= reg_size) break;
              } 
            }
            vpi_put_value(reg_handle, &reg_value, NULL, vpiNoDelay);
          }
        }
      }
      mod_iters.push(vpi_iterate(vpiModule, mod_handle));
    }
  }
  clock_t end_clock = clock();
  vpi_printf("initialization time(sec): %.2f\n", 
             double(end_clock - begin_clock) / CLOCKS_PER_SEC);
  vpi_printf("Finish initialization\n");

  return 0;
}
*/

int32_t force_regs_compiletf (char *user_data) {
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle), arg_handle;
  bool error = false;

  if (arg_iter == NULL) {
    vpi_printf("ERROR: $force_regs requires at least two argument(filename, module)\n"); 
    vpi_control(vpiFinish, 1); /* abort simulation */
    return 0;
  }

  arg_handle = vpi_scan(arg_iter);
  if (vpi_get(vpiType, arg_handle) != vpiReg) {
    vpi_printf("ERROR: $force_regs requires the first argument as a string variable(filename)\n");
    error = true;
  }

  arg_handle = vpi_scan(arg_iter);
  if (vpi_get(vpiType, arg_handle) != vpiModule) {
    vpi_printf("ERROR: $force_regs requires the second argument as a module\n");
    error = true;
  }

  if (vpi_scan(arg_iter) != NULL) {
    vpi_printf("ERROR: $force_regs requires only two arguments(filename, module))\n");
    error = true;
  }

  if (error) {
    vpi_control(vpiFinish, 1); /* abort simulation */
    return 0;
  }
  return 0; 
}
 
/*==========================================================================
                 Registration Functions
=============================================================================*/
/*
void force_regs_registration() {
  s_vpi_systf_data tf_data;
  
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$force_regs";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = force_regs_calltf;
  tf_data.compiletf = force_regs_compiletf;
  tf_data.user_data = NULL;

  vpi_register_systf(&tf_data);

  return;
}
*/

void wire_poke_registration() {
  s_vpi_systf_data tf_data;
  
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$wire_poke";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = wire_poke_calltf;
  tf_data.compiletf = NULL; // wire_poke_compiletf;
  tf_data.user_data = NULL;

  vpi_register_systf(&tf_data);

  return;
}

void wire_peek_registration() {
  s_vpi_systf_data tf_data;
  
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$wire_peek";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = wire_peek_calltf;
  tf_data.compiletf = NULL; // wire_peek_compiletf;
  tf_data.user_data = NULL;

  vpi_register_systf(&tf_data);

  return;
}

void mem_poke_registration() {
  s_vpi_systf_data tf_data;
  
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$mem_poke";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = mem_poke_calltf;
  tf_data.compiletf = NULL; // mem_poke_compiletf;
  tf_data.user_data = NULL;

  vpi_register_systf(&tf_data);

  return;
}

void mem_peek_registration() {
  s_vpi_systf_data tf_data;
  
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$mem_peek";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = mem_peek_calltf;
  tf_data.compiletf = NULL; // mem_peek_compiletf;
  tf_data.user_data = NULL;

  vpi_register_systf(&tf_data);

  return;
}

/*==========================================================================
                 Start-up Array
=============================================================================*/
void (*vlog_startup_routines[]) () = {
  wire_poke_registration,
  wire_peek_registration,
  mem_poke_registration,
  mem_peek_registration,
  0
};
