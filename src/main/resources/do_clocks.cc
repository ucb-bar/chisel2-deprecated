/* parallel clock synchronization code */

void @MODULENAME@::@DO_CLOCKS@( pt_clock_t clock_type, dat_t<1> reset ) {
	g_comp_sync_block.clock_type = clock_type;
	g_comp_sync_block.do_reset = reset.to_bool();

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
