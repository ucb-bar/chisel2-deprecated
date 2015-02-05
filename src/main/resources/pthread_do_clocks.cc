/* parallel clock synchronization code */

void @MODULENAME@::@DO_CLOCKS@( pt_clock_t clock_type, dat_t<1> reset ) {
	g_comp_sync_block.clock_type = clock_type;
	g_comp_sync_block.do_reset = reset.to_bool();

	task_sync.master_wait_ready();

	task_sync.master_work();

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
