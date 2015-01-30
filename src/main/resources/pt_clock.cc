/* parallel clock template
 * Contents generated per-thread.
 */

void @MODULENAME@::@PT_CLOCKNAME@(  ) {
    dat_t<1> reset = LIT<1>(g_comp_sync_block.do_reset);
	switch( g_comp_sync_block.clock_type ) {
	case PCT_LO: @CLOCKLONAME@( reset ); break;
	case PCT_HI: @CLOCKHINAME@( reset ); break;
	case PCT_DONE: /* FALL_THROUGH */
	default:
		break;
	}
}
