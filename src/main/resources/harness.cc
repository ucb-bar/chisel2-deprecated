/* Tester harness code.
 * This file is massaged during creation, replacing the @ ID @ tokens with generated values.
 */
int main (int argc, char* argv[]) {
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
    #if DYNAMIC_THREAD_DISPATCH
  	    extern void clock_task(MultiFIR_t * module);
		#pragma omp parallel num_threads(@NTESTTHREADSP1@)
		{
			#pragma omp single nowait
			{
				int nthreads = omp_get_num_threads();
				for (int t = 0; t < nthreads - 1; t += 1) {
					#pragma omp task
					{
						int myId = omp_get_thread_num();
						clock_task(module);
					}
				}
				#pragma omp task
				{
					api->read_eval_print_loop();
					task_sync.master_wait_ready();
					g_comp_sync_block.clock_type = PCT_DONE;
					task_sync.master_work();
				}
			}
			#pragma omp taskwait
		}
    #else
		#pragma omp parallel num_threads(@NTESTTHREADSP1@)
		{
			#pragma omp single nowait
			{
				// If we aren't using DYNAMIC_THREAD_DISPATCH, this code is duplicated once per thread.
				// TASK_START
				#pragma omp task
				{
					do {
						task_sync.worker_ready();
						task_sync.worker_wait_work();
						pt_clock_t t_clock_type;
						#pragma omp atomic read
						t_clock_type = g_comp_sync_block.clock_type;

						if (t_clock_type == PCT_DONE)
							break;
						// This will be something like:
						//    module->pt_clock_T%d( );

						@TASKCODE@

						task_sync.worker_done();
						task_sync.worker_wait_rest();
					} while(1);
				}
				// TASK_END
				// The read_eval_print_loop task.
				#pragma omp task
				{
					api->read_eval_print_loop();
					// Tell the other threads we're done.
					task_sync.master_wait_ready();
					g_comp_sync_block.clock_type = PCT_DONE;
					task_sync.master_work();
				}
			}
			#pragma omp taskwait
		}
	#endif // DYNAMIC_THREAD_DISPATCH
  #else //THREAD_MODEL != TM_OPENMP
	api->read_eval_print_loop();
	// Signal threads it's time to exit.
	g_comp_sync_block.clock_type = PCT_DONE;
	task_sync.master_wait_ready();
	task_sync.master_work();
  #endif //THREAD_MODEL == TM_OPENMP
#else
  api->read_eval_print_loop();
#endif //PERSISTENT_THREADS
  fclose(f);
  fclose(tee);
}
