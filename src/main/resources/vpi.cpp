#include "vpi.h"

/*==========================================================================
                 User Functions
=============================================================================*/

PLI_INT32 init_clks_calltf(PLI_BYTE8 *vpi_api) {
  ((vpi_api_t*)vpi_api)->init_clks();
  return 0;
}

PLI_INT32 init_rsts_calltf(PLI_BYTE8 *vpi_api) {
  ((vpi_api_t*)vpi_api)->init_rsts();
  return 0;
}

PLI_INT32 init_ins_calltf(PLI_BYTE8 *vpi_api) {
  ((vpi_api_t*)vpi_api)->init_ins();
  return 0;
}

PLI_INT32 init_outs_calltf(PLI_BYTE8 *vpi_api) {
  ((vpi_api_t*)vpi_api)->init_outs();
  return 0;
}

PLI_INT32 init_sigs_calltf(PLI_BYTE8 *vpi_api) {
  ((vpi_api_t*)vpi_api)->init_sigs();
  return 0;
}

PLI_INT32 tick_calltf(PLI_BYTE8 *vpi_api) {
  ((vpi_api_t*)vpi_api)->tick();
  return 0;
}

PLI_INT32 tick_cb(p_cb_data cb_data) {
  ((vpi_api_t*)cb_data->user_data)->tick();
  return 0;
}

/*==========================================================================
                 Registration Functions
=============================================================================*/
void registration() {
  vpi_api_t* vpi_api = new vpi_api_t;

  s_vpi_systf_data tf_data;
  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$init_clks";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = init_clks_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data = (PLI_BYTE8*) vpi_api;
  vpi_register_systf(&tf_data);

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$init_rsts";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = init_rsts_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data = (PLI_BYTE8*) vpi_api;
  vpi_register_systf(&tf_data);

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$init_ins";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = init_ins_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data =(PLI_BYTE8*) vpi_api;
  vpi_register_systf(&tf_data);

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$init_outs";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = init_outs_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data =(PLI_BYTE8*) vpi_api;
  vpi_register_systf(&tf_data);

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$init_sigs";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = init_sigs_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data =(PLI_BYTE8*) vpi_api;
  vpi_register_systf(&tf_data);

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = (PLI_BYTE8*) "$tick";
  tf_data.sizetf    = NULL;
  tf_data.calltf    = tick_calltf;
  tf_data.compiletf = NULL;
  tf_data.user_data =(PLI_BYTE8* )vpi_api;
  vpi_register_systf(&tf_data);
  return;
}

/*==========================================================================
                 Start-up Array
=============================================================================*/
void (*vlog_startup_routines[]) () = {
  registration,
  0
};

