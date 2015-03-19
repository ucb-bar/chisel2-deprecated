/*
 * chisel_sync_none.h
 *
 *  Created on: Feb 23, 2015
 *      Author: jrl
 */

#ifndef CHISEL_SYNC_NONE_H_
#define CHISEL_SYNC_NONE_H_

#include "chisel_sync.h"

class chisel_sync_none: public chisel_sync {
protected:
	typedef chisel_sync parent;
public:
	chisel_sync_none(int threadCount);
	virtual ~chisel_sync_none();

	void worker_ready();
	void worker_wait_work ();
	void worker_done();
	void worker_wait_rest();
	void master_wait_ready();
	void master_work();
	void master_wait_done();
	void master_rest();
};

#endif /* CHISEL_SYNC_NONE_H_ */
