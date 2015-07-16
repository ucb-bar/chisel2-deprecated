#include <iostream>
#include <vector>
#include "vpi_user.h"

/*==========================================================================
                 User Functions
=============================================================================*/
struct {
  std::vector<vpiHandle> resets;
  std::vector<vpiHandle> inputs;
  std::vector<vpiHandle> outputs;
} sim_data; 

PLI_INT32 init_rsts_calltf(PLI_BYTE8 *user_data) {
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
  // Cache Resets
  while (vpiHandle arg_handle = vpi_scan(arg_iter)) {
    sim_data.resets.push_back(arg_handle);
  }
  return 0;
}

PLI_INT32 init_ins_calltf(PLI_BYTE8 *user_data) {
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
  // Cache Inputs
  while (vpiHandle arg_handle = vpi_scan(arg_iter)) {
    sim_data.inputs.push_back(arg_handle);
  }
  return 0;
}

PLI_INT32 init_outs_calltf(PLI_BYTE8 *user_data) {
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle arg_iter = vpi_iterate(vpiArgument, syscall_handle);
  // Cache Outputs
  while (vpiHandle arg_handle = vpi_scan(arg_iter)) {
    sim_data.outputs.push_back(arg_handle);
  }
  return 0;
}


typedef enum SIM_CMD { RESET, STEP, FIN } SIM_CMD;

void reset() {
  for (size_t i = 0 ; i < sim_data.resets.size() ; i++) {
    s_vpi_value value_s;
    value_s.format = vpiHexStrVal;
    value_s.value.str = (PLI_BYTE8*) "1";
    vpi_put_value(sim_data.resets[i], &value_s, NULL, vpiNoDelay);  
  }
}

void start() {
  for (size_t i = 0 ; i < sim_data.resets.size() ; i++) {
    s_vpi_value value_s;
    value_s.format = vpiHexStrVal;
    value_s.value.str = (PLI_BYTE8*) "0";
    vpi_put_value(sim_data.resets[i], &value_s, NULL, vpiNoDelay);  
  }
}

void generate_outputs() {
  for (size_t i = 0 ; i < sim_data.outputs.size() ; i++) {
    s_vpi_value value_s;
    value_s.format = vpiHexStrVal;
    vpi_get_value(sim_data.outputs[i], &value_s);
    fprintf(stdout, "%s\n", value_s.value.str);
  }
}

void consume_inputs() {
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

PLI_INT32 tick_calltf(PLI_BYTE8 *user_data) {
  vpiHandle syscall_handle = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle test_handle = vpi_scan(vpi_iterate(vpiModule, NULL));
  vpiHandle top_handle = vpi_scan(vpi_iterate(vpiModule, test_handle));
  static bool is_reset = false;
  
  // First, Generates output tokens  (in hex)
  generate_outputs();
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
      case RESET: 
        reset();
        is_reset = true;
        exit = true;
        break;
      case STEP: 
        consume_inputs(); 
        exit = true; 
        break;
      case FIN: 
        vpi_control(vpiFinish, 0); 
        exit = true; 
        break;
      default:
        break;
    }
  } while (!exit);

  return 0;
}

/*==========================================================================
                 Registration Functions
=============================================================================*/
void init_rsts_registration() {
  s_vpi_systf_data tf_data;
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$init_rsts";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = init_rsts_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data = NULL;
  vpi_register_systf(&tf_data);
  return;
}

void init_ins_registration() {
  s_vpi_systf_data tf_data;
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$init_ins";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = init_ins_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data = NULL;
  vpi_register_systf(&tf_data);
  return;
}

void init_outs_registration() {
  s_vpi_systf_data tf_data;
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$init_outs";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = init_outs_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data = NULL;
  vpi_register_systf(&tf_data);
  return;
}

void tick_registration() {
  s_vpi_systf_data tf_data;
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$tick";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = tick_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data = NULL;
  vpi_register_systf(&tf_data);
  return;
}

/*==========================================================================
                 Start-up Array
=============================================================================*/
void (*vlog_startup_routines[]) () = {
  init_rsts_registration,
  init_ins_registration,
  init_outs_registration,
  tick_registration,
  0
};
