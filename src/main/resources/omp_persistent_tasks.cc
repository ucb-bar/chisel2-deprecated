/* This template provides a persistent-task OpenMP implementation. */
int counts[@NTESTTHREADSP1@];
#pragma omp parallel num_threads(@NTESTTHREADSP1@)
{
	#pragma omp single nowait
	{
		g_comp_sync_block.nthreads = @NTESTTHREADSP1@ - 1;
		g_comp_sync_block.clock_done = 0;
		g_comp_sync_block.do_clock = false;
		// TASK_START
		#pragma omp task
		{
			int myId = omp_get_thread_num();
			int myCount = 0;
			do {
				int t_do_clock = 0;
				while(1) {
					#pragma omp flush (g_comp_sync_block)
					#pragma omp atomic read
					t_do_clock = g_comp_sync_block.do_clock;

					if (t_do_clock == 1)
					  break;
				}
				pt_clock_t t_clock_type;
				#pragma omp atomic read
				t_clock_type = g_comp_sync_block.clock_type;

				if (t_clock_type == PCT_DONE)
					break;

				@TASKCODE@

				myCount += 1;
				counts[myId] = myCount;

				#pragma omp flush
				#pragma omp critical
				g_comp_sync_block.clock_done++;
				#pragma omp flush (g_comp_sync_block)

				while(1) {
					#pragma omp flush (g_comp_sync_block)
					#pragma omp atomic read
					t_do_clock = g_comp_sync_block.do_clock;

					if (t_do_clock == 0)
					  break;
				}

				#pragma omp critical
				g_comp_sync_block.clock_done -= 1;
				#pragma omp flush (g_comp_sync_block)

			} while(1);
		}
		// TASK_END
	}
	#pragma omp taskwait
}
