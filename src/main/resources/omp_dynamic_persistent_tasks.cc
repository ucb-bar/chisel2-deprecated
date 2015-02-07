/* This template provides a dynamic persistent-task OpenMP implementation. */
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))
extern comp_current_clock_t g_current_clock;

void @MODULENAME@::call_clock_code(dat_t<1> reset)
{
	clock_code_t clock_code = NULL;
	int next_index, this_index;
	do {
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
			clock_code = g_current_clock.methods->clock_codes[this_index];
			(this->*clock_code)(reset);
		} else {
			clock_code = NULL;
		}
	} while (clock_code != NULL);
}

void clock_task(@MODULENAME@ * module)
{
	int cycles = 0;
	do {
		clock_code_t clock_code;
		pt_clock_t t_clock_type;
		cycles += 1;
		task_sync.worker_ready();
		task_sync.worker_wait_work();
		#pragma omp flush(g_comp_sync_block)
	    dat_t<1> reset = LIT<1>(g_comp_sync_block.do_reset);

		#pragma omp atomic read
		t_clock_type = g_comp_sync_block.clock_type;
		if (t_clock_type == PCT_DONE)
			return;
		module->call_clock_code(reset);

		#pragma omp flush

		task_sync.worker_done();

		task_sync.worker_wait_rest();


	} while(1);
}
