/*
 * chisel_sync_omp.h
 *
 *  Created on: Jan 23, 2015
 *      Author: jrl
 */

#ifndef CHISEL_SYNC_OMP_H_
#define CHISEL_SYNC_OMP_H_

#include "chisel_sync.h"
#include <omp.h>
#include <vector>

class chisel_sync_omp: public chisel_sync {
protected:
	typedef chisel_sync parent;

public:
	chisel_sync_omp(int threads) : chisel_sync(threads) {}
	virtual ~chisel_sync_omp() {}
	void master_wait_ready() {}
	void worker_ready() {}
	void worker_wait_work () {
		#pragma omp barrier
	}
	void master_work() {
		parent::master_work();
//		#pragma omp flush(do_work)
		#pragma omp barrier
	}
	void master_wait_done() {
		#pragma omp barrier
	}
	void worker_done() {
		#pragma omp barrier
	}
	void worker_wait_rest() {}
	void master_rest() {
		parent::master_rest();
//		#pragma omp flush(do_work)
	}
};

#endif /* CHISEL_SYNC_OMP_H_ */
