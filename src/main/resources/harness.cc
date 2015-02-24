/* Tester harness code.
 * This file is massaged during creation, replacing the @ ID @ tokens with generated values.
 */
#include <iostream>
#include <fstream>
#include <cstdio>

int main (int argc, char* argv[]) {
	istream * cmdin = &cin;
	ifstream * is = NULL;
	for (int i = 1; i < argc; i += 1) {
	  if (argv[i][0] == '-') {
		  char * option = argv[i];
		  if (strcmp(option, "-i") == 0) {
			  is = new ifstream (argv[i+1], std::ios::in);
			  if (is == NULL || !is->is_open()) {
				  cerr << "Can't open " <<  argv[i+1] << "for reading: " << strerror(errno) << std::endl;
				  return(1);
			  } else {
				  cmdin = is;
			  }
		  }
		  i += 1;
	  }
	}
  @MODULENAME@ * module = new @MODULENAME@();
  module->init();
  @APINAME@ * api = new @APINAME@();
  api->init(module);
  FILE *f = @VCDCODE@;
  FILE *tee = @DUMPTESTINPUTCODE@;
  module->set_dumpfile(f);
  api->set_teefile(tee);
  // If we're using OpenMP and persistent threads, we need to put support here.
  // Due to the use of OpenMP's block-structured #pragmas,
  //  the major parallel code has to appear within the same block.
#if PERSISTENT_THREADS
  extern chisel_sync_@SYNCCLASS@ task_sync;
  extern comp_sync_block g_comp_sync_block;
  #if THREAD_MODEL == TM_OPENMP
    extern void clock_task(MultiFIR_t * module, int task_no);
    #if DYNAMIC_THREAD_DISPATCH
		#pragma omp parallel num_threads(@NTESTTASKS@)
		{
			#pragma omp single nowait
			{
				int nthreads = omp_get_num_threads();
				for (int t = 0; t < nthreads - 1; t += 1) {
					#pragma omp task
					{
						int myId = omp_get_thread_num();
						clock_task(module, t);
					}
				}
				#pragma omp task
				{
					api->read_eval_print_loop(*cmdin);
					task_sync.master_wait_ready();
					g_comp_sync_block.clock_type = PCT_DONE;
					task_sync.master_work();
				}
			}
			#pragma omp taskwait
		}
    #else
		#pragma omp parallel sections num_threads(@NTESTTASKS@)
		{
			// If we aren't using DYNAMIC_THREAD_DISPATCH, this code is duplicated once per thread.
			// TASK_START
			#pragma omp section
			{
				int myId = omp_get_thread_num();
				clock_task(module, myId);
			}
			// TASK_END
			// The read_eval_print_loop task.
			#pragma omp section
			{
				api->read_eval_print_loop(*cmdin);
				task_sync.master_wait_ready();
				g_comp_sync_block.clock_type = PCT_DONE;
				task_sync.master_work();
			}
		}
	#endif // DYNAMIC_THREAD_DISPATCH
  #else //THREAD_MODEL != TM_OPENMP
	api->read_eval_print_loop(*cmdin);
	// Signal threads it's time to exit.
	g_comp_sync_block.clock_type = PCT_DONE;
	task_sync.master_wait_ready();
	task_sync.master_work();
  #endif //THREAD_MODEL == TM_OPENMP
#else
  api->read_eval_print_loop(*cmdin);
#endif //PERSISTENT_THREADS
  if (f) fclose(f);
  if (tee) fclose(tee);
  if (is) {
	  if (is->is_open())
		  is->close();
	  delete is;
  }
}
