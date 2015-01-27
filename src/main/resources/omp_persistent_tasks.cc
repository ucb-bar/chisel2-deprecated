/* This template provides a persistent-task OpenMP implementation. */

#pragma omp parallel num_threads(@NTESTTHREADSP1@)
{
	#pragma omp single nowait
	{
		extern chisel_sync_omp task_sync;
		// TASK_START
		#pragma omp task
		{
			do {
				task_sync.worker_wait_ready();
				pt_clock_t t_clock_type;
				#pragma omp atomic read
				t_clock_type = g_comp_sync_block.clock_type;

				if (t_clock_type == PCT_DONE)
					break;

				@TASKCODE@

				task_sync.worker_done();

				task_sync.worker_wait_done();

			} while(1);
		}
		// TASK_END
	}
	#pragma omp taskwait
}
