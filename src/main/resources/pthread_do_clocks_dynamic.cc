/* parallel clock synchronization code */
extern comp_current_clock_t g_current_clock;
extern comp_clock_methods_t g_comp_clocks[];
extern pthread_mutex_t g_clock_code_mutex;

void @MODULENAME@::@DO_CLOCKS@( pt_clock_t clock_type, dat_t<1> reset ) {
	g_comp_sync_block.clock_type = clock_type;
	g_comp_sync_block.do_reset = reset.to_bool();

	pthread_mutex_lock(&g_clock_code_mutex);
	g_current_clock.index = 0;
    switch (clock_type) {
    case PCT_LO:
    	g_current_clock.methods = &g_comp_clocks[0];
		break;

    case PCT_HI:
    	g_current_clock.methods = &g_comp_clocks[1];
    	break;

    case PCT_XHI:
    	g_current_clock.methods = &g_comp_clocks[2];
    	break;
    }
	pthread_mutex_unlock(&g_clock_code_mutex);

	task_sync.master_wait_ready();

	task_sync.master_work();

	this->call_clock_code(reset);

	task_sync.master_wait_done();

	task_sync.master_rest();
}

void @MODULENAME@::clock_lo( dat_t<1> reset ) {
	@DO_CLOCKS@( PCT_LO, reset );
}

void @MODULENAME@::clock_hi( dat_t<1> reset ) {
	@DO_CLOCKS@( PCT_HI, reset );
	@DO_CLOCKS@( PCT_XHI, reset );
}
