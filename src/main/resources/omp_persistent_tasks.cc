/* This template provides a persistent-task OpenMP implementation. */
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))

typedef void(@MODULENAME@::*pt_clock_code_t)(void);
static @MODULENAME@ * clock_thread_module;

const int nthreads = @NTESTTHREADS@;
static struct clock_thread {
	int status;
    pthread_t id;
    pt_clock_code_t thread_pt_clock_code;
} threads[nthreads] = { @INIT_THREADS_ARRAY@ };

void clock_task(@MODULENAME@ * module, int task_no)
{
	int cycles = 0;
	const pt_clock_code_t call_pt_clock_code = threads[task_no].thread_pt_clock_code;
	do {
		clock_code_t clock_code;
		pt_clock_t t_clock_type;
		cycles += 1;
		task_sync.worker_ready();
		task_sync.worker_wait_work();
		#pragma omp flush(g_comp_sync_block)
		#pragma omp atomic read
		t_clock_type = g_comp_sync_block.clock_type;
		if (t_clock_type == PCT_DONE)
			return;
		(module->*call_pt_clock_code)( );

		#pragma omp flush

		task_sync.worker_done();

		task_sync.worker_wait_rest();


	} while(1);
}
