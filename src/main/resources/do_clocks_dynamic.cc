/* parallel clock synchronization code */
extern comp_current_clock_t g_current_clock;
extern comp_clock_methods_t g_comp_clocks[];

void @MODULENAME@::@DO_CLOCKS@( pt_clock_t clock_type, dat_t<1> reset ) {
	g_comp_sync_block.clock_type = clock_type;
	g_comp_sync_block.do_reset = reset.to_bool();
    #pragma omp flush(g_comp_sync_block)

	g_current_clock.index = 0;
    switch (clock_type) {
    case PCT_LO:
    	g_current_clock.methods = &g_comp_clocks[0];
		break;

    case PCT_HII:
    	g_current_clock.methods = &g_comp_clocks[1];
    	break;

    case PCT_HIX:
    	g_current_clock.methods = &g_comp_clocks[2];
		break;
    }
	#pragma omp flush(g_current_clock)

	task_sync.master_work(true);

	task_sync.master_wait_work(true);

	task_sync.master_work(false);

	task_sync.master_wait_work(false);
}

void @MODULENAME@::clock_lo( dat_t<1> reset ) {
	@DO_CLOCKS@( PCT_LO, reset );
}

void @MODULENAME@::clock_hi( dat_t<1> reset ) {
	@DO_CLOCKS@( PCT_HII, reset );
	@DO_CLOCKS@( PCT_HIX, reset );
}
