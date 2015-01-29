/* This template provides a dynamic persistent-task OpenMP implementation. */
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))
extern comp_current_clock_t g_current_clock;

clock_code_t next_clock_code()
{
	clock_code_t result = NULL;
	int next_index, this_index;
	#pragma omp flush(g_current_clock)
	#pragma omp critical (index_increment)
	{
		next_index = g_current_clock.index + 1;
		#pragma atomic write
		g_current_clock.index = next_index;
		#pragma omp flush(g_current_clock)
	}
	this_index = next_index - 1;
	if (this_index <= g_current_clock.methods->index_max) {
		result = g_current_clock.methods->clock_codes[this_index];
	}
	return result;
}

void clock_task(@MODULENAME@ * module)
{
	int cycles = 0;
	do {
		clock_code_t clock_code;
		pt_clock_t t_clock_type;
		cycles += 1;
		task_sync.worker_wait_ready();
		#pragma omp flush(g_comp_sync_block)
	    dat_t<1> reset = LIT<1>(g_comp_sync_block.do_reset);

		#pragma omp atomic read
		t_clock_type = g_comp_sync_block.clock_type;
		if (t_clock_type == PCT_DONE)
			return;
		while ((clock_code = next_clock_code()) != NULL) {
			CALL_MEMBER_FN((*module), clock_code)(reset);
		}

		#pragma omp flush

		task_sync.worker_done();

		task_sync.worker_wait_done();


	} while(1);
}

// AFTERMAINCODE_START
    #pragma omp parallel num_threads(@NTESTTHREADSP1@)
	{
		#pragma omp single nowait
		{
			int nthreads = omp_get_num_threads();
			for (int t = 0; t < nthreads - 1; t += 1) {
				// TASK_START
				#pragma omp task
				{
					clock_task(module);
				}
				// TASK_END
			}
			#pragma omp task
			{
			  @REPLCODE@
			}
		}
		#pragma omp taskwait
	}
// AFTERMAINCODE_END

