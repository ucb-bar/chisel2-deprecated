/* This template provides a dynamic persistent-task OpenMP implementation. */
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))
extern comp_clocks_t g_comp_clocks;

clock_code_t next_clock_code(pt_clock_t clock_type)
{
	clock_code_t result = NULL;
	int next_index, this_index;
	switch (clock_type) {
	default:
	case PCT_DONE:
		break;
	case PCT_LO:
		#pragma omp critical (index_increment)
		{
			next_index = g_comp_sync_block.index_lo + 1;
			#pragma atomic write
			g_comp_sync_block.index_lo = next_index;
			#pragma omp flush(g_comp_sync_block)
		}
		this_index = next_index - 1;
		if (this_index <= g_comp_clocks.lo.index_max) {
			result = g_comp_clocks.lo.clock_codes[this_index];
		}
		break;
	case PCT_HI:
		#pragma omp critical (index_increment)
		{
			next_index = g_comp_sync_block.index_hi + 1;
			#pragma atomic write
			g_comp_sync_block.index_hi = next_index;
			#pragma omp flush(g_comp_sync_block)
		}
		this_index = next_index - 1;
		if (this_index <= g_comp_clocks.hi.index_max) {
			result = g_comp_clocks.hi.clock_codes[this_index];
		}
		break;
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
		t_clock_type = g_comp_sync_block.clock_type;
	    dat_t<1> reset = LIT<1>(g_comp_sync_block.do_reset);

		if (t_clock_type == PCT_DONE) {
			break;
		} else {
			while ((clock_code = next_clock_code(t_clock_type)) != NULL) {
				CALL_MEMBER_FN((*module), clock_code)(reset);
			}
		}

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

