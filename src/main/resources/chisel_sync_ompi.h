/*
 * chisel_sync_ompi.h
 *
 *  Created on: Jan 23, 2015
 *      Author: jrl
 */

#ifndef CHISEL_SYNC_OMPI_H_
#define CHISEL_SYNC_OMPI_H_

#include "chisel_sync.h"

class chisel_sync_ompi: public chisel_sync {
protected:
	typedef chisel_sync parent;

public:
	chisel_sync_ompi(int theThreads);
	virtual ~chisel_sync_ompi();

	void worker_wait_work ();
	void worker_done();
	void worker_wait_rest();
	void master_wait_ready();
	void master_work();
	void master_wait_done();
	void master_rest();
};

#endif /* CHISEL_SYNC_OMPI_H_ */
