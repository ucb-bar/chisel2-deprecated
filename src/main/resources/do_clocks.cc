/* parallel clock synchronization code */

void @MODULENAME@::@DO_CLOCKS@( pt_clock_t clock_type, dat_t<1> reset ) {
	g_comp_sync_block.clock_type = clock_type;
	g_comp_sync_block.do_reset = reset.to_bool();
	switch (clock_type) {
	case PCT_LO:	g_comp_sync_block.index_lo = 0; break;
	case PCT_HI:	g_comp_sync_block.index_hi = 0; break;
	}
	task_sync.master_work(true);

	task_sync.master_wait_work(true);

	task_sync.master_work(false);

	task_sync.master_wait_work(false);
}

void @MODULENAME@::clock_lo( dat_t<1> reset ) {
	@DO_CLOCKS@( PCT_LO, reset );
}

void @MODULENAME@::clock_hi( dat_t<1> reset ) {
	@DO_CLOCKS@( PCT_HI, reset );
}
