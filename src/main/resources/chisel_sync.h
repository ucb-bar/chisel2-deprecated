/*
 * chisel_sync.h
 *
 *  Created on: Jan 23, 2015
 *      Author: jrl
 */

#ifndef CHISEL_SYNC_H_
#define CHISEL_SYNC_H_

#include <assert.h>

class chisel_sync {
protected:
	const int nthreads;				// Number of threads executing.
	const int nWorkers;				// Number of worker threads.
	int done;						// Number of threads that have finished their work
	int ready;						// Number of threads that are ready to work
	bool do_work;

public:
	chisel_sync(int threadCount) : nthreads(threadCount), nWorkers(threadCount - 1), done(0), ready(0), do_work(false) {}
	virtual ~chisel_sync() {}
	virtual void worker_ready() { ready += 1; }
	virtual void worker_wait_work() = 0;
	virtual void worker_done() { done += 1; }
	virtual void worker_wait_rest() = 0;
	virtual void master_wait_ready() = 0;
	virtual void master_work() { ready = 0; done = 0; do_work = true; }
	virtual void master_wait_done() = 0;
	virtual void master_rest() { do_work = false; }
};




#endif /* CHISEL_SYNC_H_ */
