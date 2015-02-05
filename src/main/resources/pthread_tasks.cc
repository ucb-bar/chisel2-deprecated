/* This template provides a persistent-task pthreads implementation. */

	for (t = 0; t < @NTESTTHREADS@; t += 1) {
		err =  pthread_create(&threads[t].thread, NULL, pthread_task, &threads[t]);
	}

void pthread_task(task_def *tdp)
{
	do {
		task_sync.wait_ready();
		pt_clock_t t_clock_type;
		t_clock_type = g_comp_sync_block.clock_type;

		if (t_clock_type == PCT_DONE)
			break;

		tdp->clockCode();

		task_sync.done();

		task_sync.wait_done();

	} while(1);
}
