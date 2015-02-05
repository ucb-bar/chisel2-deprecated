/* This template provides a dynamic persistent-task pthread implementation. */
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))
extern comp_current_clock_t g_current_clock;
pthread_mutex_t g_clock_code_mutex = PTHREAD_MUTEX_INITIALIZER;

void @MODULENAME@::call_clock_code(dat_t<1> reset)
{
	clock_code_t clock_code = NULL;
	int next_index, this_index;
	do {
		pthread_mutex_lock(&g_clock_code_mutex);
		{
			next_index = g_current_clock.index + 1;
			g_current_clock.index = next_index;
		}
		pthread_mutex_unlock(&g_clock_code_mutex);
		this_index = next_index - 1;
		if (this_index <= g_current_clock.methods->index_max) {
			clock_code = g_current_clock.methods->clock_codes[this_index];
			(this->*clock_code)(reset);
		} else {
			clock_code = NULL;
		}
	} while (clock_code != NULL);
}

void * clock_task(void * arg)
{
	@MODULENAME@ * module = (@MODULENAME@ *) arg;
	int cycles = 0;
	do {
		clock_code_t clock_code;
		pt_clock_t t_clock_type;
		cycles += 1;
		task_sync.worker_ready();
		task_sync.worker_wait_work();
	    dat_t<1> reset = LIT<1>(g_comp_sync_block.do_reset);

		t_clock_type = g_comp_sync_block.clock_type;
		if (t_clock_type == PCT_DONE)
			break;
		module->call_clock_code(reset);

		task_sync.worker_done();

		task_sync.worker_wait_rest();


	} while(1);
	return NULL;
}

// AFTERMAINCODE_START
	{
	    const int nthreads = @NTESTTHREADS@;
	    struct chisel_threads {
	    	int status;
	        pthread_t id;
	    } threads[nthreads];
		for (int t = 0; t < nthreads; t += 1) {
			// TASK_START
			{
				threads[t].status = pthread_create(&threads[t].id, NULL, &clock_task, module);
			}
			// TASK_END
		}
		{
		  @REPLCODE@
		}
	}
// AFTERMAINCODE_END

