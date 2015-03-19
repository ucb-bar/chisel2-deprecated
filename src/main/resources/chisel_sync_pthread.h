/*
 * chisel_sync_pthread.h
 *
 *  Created on: Jan 23, 2015
 *      Author: jrl
 */

#ifndef CHISEL_SYNC_PTHREAD_H_
#define CHISEL_SYNC_PTHREAD_H_

#include "chisel_sync.h"
#include <pthread.h>

class chisel_sync_pthread: public chisel_sync {
protected:
	typedef chisel_sync parent;
	struct cond_var {
		static const int nograb = 0;
		static const int nohold = 0;
		static const int grab = 1;
		static const int hold = 2;
		pthread_cond_t cond;
		pthread_mutex_t mutex;
		int value;
		cond_var();
		void lock();
		void unlock();
		void post(int a_value, int lock_state);
		int wait(int lock_state);
		void broadcast(int a_value, int lock_state);
		pthread_t owner;
		int posts;
		int waits;
		int wakes;
	};
	cond_var ready_cond;		// Signalled by workers, waited on by master
	cond_var done_cond;			// Signalled by workers, waited on by master
	cond_var work_cond;			// Signalled by master, waited on by workers
	cond_var rest_cond;			// Signalled by master, waited on by workers
	int waiting_for_ready;
	int waiting_for_work;
	int waiting_for_done;
	int waiting_for_rest;

public:
	chisel_sync_pthread(int threads);
	virtual ~chisel_sync_pthread();

	void worker_ready();
	void worker_wait_work ();
	void worker_done();
	void worker_wait_rest();
	void master_wait_ready();
	void master_work();
	void master_wait_done();
	void master_rest();
};

#endif /* CHISEL_SYNC_PTHREAD_H_ */
