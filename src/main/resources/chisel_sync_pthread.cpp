/*
 * chisel_sync_pthread.cpp
 *
 *  Created on: Jan 23, 2015
 *      Author: jrl
 */

#include "chisel_sync_pthread.h"
#include <assert.h>

#define WORK_BARRIER 1
#define DONE_BARRIER 1
#define READY_BARRIER 1

chisel_sync_pthread::cond_var::cond_var() : value(0), owner(0), posts(0), waits(0), wakes(0) {
	pthread_cond_init(&cond, NULL);
	pthread_mutex_init(&mutex, NULL);
}

inline void chisel_sync_pthread::cond_var::lock() {
	int err = 0;
	do {
		err = pthread_mutex_lock(&mutex);
		if (err != 0)
			break;
		owner = pthread_self();
	} while(0);
	assert(err == 0);
}

inline void chisel_sync_pthread::cond_var::unlock() {
	int err = 0;
	do {
		owner = 0;
		err = pthread_mutex_unlock(&mutex);
		if (err != 0)
			break;
	} while(0);
	assert(err == 0);
}

inline void chisel_sync_pthread::cond_var::post(int a_value, int lock_state) {
	int err = 0;
	do {
		if (lock_state & grab)
			lock();
		value = a_value;
		posts += 1;
		err = pthread_cond_signal(&cond);
		if (err != 0)
			break;
		if (!(lock_state & hold))
			unlock();
	} while(0);
	assert(err == 0);
}

int chisel_sync_pthread::cond_var::wait(int lock_state) {
	int err = 0;
	int the_value;
	do {
		if (lock_state & grab)
			lock();
		waits += 1;
		err = pthread_cond_wait(&cond, &mutex);
		the_value = value;
		wakes += 1;
		if (!(lock_state & hold))
			unlock();
	} while(0);
	assert(err == 0);
	return(the_value);
}

void chisel_sync_pthread::cond_var::broadcast(int a_value, int lock_state) {
	int err = 0;
	do {
		if (lock_state & grab)
			lock();
		value = a_value;
		posts += 1;
		err = pthread_cond_broadcast(&cond);
		if (!(lock_state & hold))
			unlock();
	} while(0);
	assert(err == 0);
}

chisel_sync_pthread::chisel_sync_pthread(int threads) :
		parent(threads),
		waiting_for_ready(0),
		waiting_for_work(0),
		waiting_for_done(0),
		waiting_for_rest(0)
{
}

void chisel_sync_pthread::worker_ready ()
{
#if READY_BARRIER
	ready_cond.lock();
#endif // READY_BARRIER
	parent::worker_ready();
#if READY_BARRIER
	ready_cond.unlock();
#endif // READY_BARRIER
}

void chisel_sync_pthread::worker_wait_work ()
{
#if WORK_BARRIER
	bool t_do_work = false;

	do {
		work_cond.lock();
#if READY_BARRIER
		ready_cond.lock();
#endif // READY_BARRIER
		waiting_for_work += 1;
#if READY_BARRIER
		if (waiting_for_work == nWorkers) {
			ready_cond.post(waiting_for_work, cond_var::nograb|cond_var::hold);
		}
		ready_cond.unlock();
#endif // READY_BARRIER
		work_cond.wait(cond_var::nograb|cond_var::hold);
		waiting_for_work -= 1;
		t_do_work = do_work;
		work_cond.unlock();
		assert(t_do_work);
	} while (!t_do_work);
#endif // WORK_BARRIER
}

/* Wait for all threads to become ready (and waiting on the work condition variable),
 * before telling any of them to work.
 */
void chisel_sync_pthread::master_wait_ready()
{
#if READY_BARRIER
	int readies = 0;
	do {
		ready_cond.lock();
		readies = waiting_for_work;
		if (readies != nWorkers) {
			waiting_for_ready += 1;
			ready_cond.wait(cond_var::nograb|cond_var::hold);
			waiting_for_ready -= 1;
		}
		ready_cond.unlock();
	} while(readies != nWorkers);
#endif // READY_BARRIER
}

void chisel_sync_pthread::master_work()
{
	work_cond.lock();
	parent::master_work();
	work_cond.broadcast(true, cond_var::nograb|cond_var::nohold);
}

void chisel_sync_pthread::master_wait_done()
{
#if DONE_BARRIER
	int t_done = 0;
	do {
		done_cond.lock();
		t_done = done;
		if (t_done != nWorkers) {
			waiting_for_done += 1;
			done_cond.wait(cond_var::nograb|cond_var::hold);
			waiting_for_done -= 1;
		}
		done_cond.unlock();
	} while(t_done != nWorkers);
#endif // DONE_BARRIER
}

void chisel_sync_pthread::master_rest()
{
	done_cond.lock();
#if REST_BARRIER
	rest_cond.lock();
#endif // REST_BARRIER
	parent::master_rest();
#if REST_BARRIER
	rest_cond.broadcast(true, cond_var::nograb|cond_var::nohold);
#endif // REST_BARRIER
	done_cond.unlock();
}

void chisel_sync_pthread::worker_done()
{
	done_cond.lock();
	parent::worker_done();
#if DONE_BARRIER && !REST_BARRIER
	if (done == nWorkers) {
		done_cond.post(waiting_for_done, cond_var::nograb|cond_var::hold);
	}
#endif // REST_BARRIER && !REST_BARRIER
	done_cond.unlock();
}

void chisel_sync_pthread::worker_wait_rest()
{
#if REST_BARRIER
	bool t_do_work = false;

	do {
		rest_cond.lock();
		done_cond.lock();
		waiting_for_rest += 1;
		if (waiting_for_rest == nWorkers) {
			done_cond.post(waiting_for_rest, cond_var::nograb|cond_var::hold);
		}
		done_cond.unlock();
		rest_cond.wait(cond_var::nograb|cond_var::hold);
		waiting_for_rest -= 1;
		t_do_work = do_work;
		rest_cond.unlock();
		assert(!t_do_work);
	} while (t_do_work);
#endif // REST_BARRIER
}

chisel_sync_pthread::~chisel_sync_pthread() {
	// TODO Auto-generated destructor stub
}

