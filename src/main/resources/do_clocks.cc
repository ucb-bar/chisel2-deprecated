/* parallel clock synchronization code */
#include <assert.h>

void @MODULENAME@::@DO_CLOCKS@( pt_clock_t clock_type, dat_t<1> reset ) {
	assert(g_comp_sync_block.clock_done == 0 && !g_comp_sync_block.do_clock);
	g_comp_sync_block.clock_type = clock_type;
	g_comp_sync_block.do_reset = reset.to_bool();
	g_comp_sync_block.clock_done = 0;
	g_comp_sync_block.do_clock = true;
	#pragma omp flush(g_comp_sync_block)

	int nthreads_done = 0;
	do {
		#pragma omp atomic read
		nthreads_done = g_comp_sync_block.clock_done;
	} while(nthreads_done != g_comp_sync_block.nthreads);

	g_comp_sync_block.do_clock = false;
	#pragma omp flush(g_comp_sync_block)

	do {
		#pragma omp atomic read
		nthreads_done = g_comp_sync_block.clock_done;
	} while(nthreads_done != 0);
}

void @MODULENAME@::clock_lo( dat_t<1> reset ) {
	@DO_CLOCKS@( PCT_LO, reset );
}

void @MODULENAME@::clock_hi( dat_t<1> reset ) {
	@DO_CLOCKS@( PCT_HI, reset );
}
