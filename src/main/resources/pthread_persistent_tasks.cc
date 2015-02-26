/* This template provides a persistent-task pthreads implementation. */
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))
typedef void(@MODULENAME@::*pt_clock_code_t)(void);
static @MODULENAME@ * clock_thread_module;

const int nthreads = @NTESTTASKS@;
static struct clock_thread {
	int status;
    pthread_t id;
    pt_clock_code_t thread_pt_clock_code;
} threads[nthreads] = { @INIT_THREADS_ARRAY@ };

void * clock_task(void * arg)
{
	pthread_t myId = pthread_self();
	const clock_thread * pct = (clock_thread *) arg;
	const pt_clock_code_t call_pt_clock_code = pct->thread_pt_clock_code;
	@MODULENAME@ * module = clock_thread_module;
	int cycles = 0;
	do {
		clock_code_t clock_code;
		pt_clock_t t_clock_type;
		cycles += 1;
		task_sync.worker_ready();
		task_sync.worker_wait_work();
//	    dat_t<1> reset = LIT<1>(g_comp_sync_block.do_reset);

		t_clock_type = g_comp_sync_block.clock_type;
		if (t_clock_type == PCT_DONE)
			break;
		(module->*call_pt_clock_code)();

		task_sync.worker_done();
		task_sync.worker_wait_rest();
	} while(1);
	return NULL;
}

static void start_clock_threads(@MODULENAME@ * module)
{
	clock_thread_module = module;
	// The first thread slot is reserved for the master thread.
	for (int t = 1; t < nthreads; t += 1) {
		// Ensure we only do this once.
		if (threads[t].id == 0 || pthread_kill(threads[t].id, 0) != 0) {
			threads[t].status = pthread_create(&threads[t].id, NULL, &clock_task, &threads[t]);
		}
	}
}
