/*
 * chisel_sync_none.cpp
 *
 *  Created on: Feb 23, 2015
 *      Author: jrl
 */

#include "chisel_sync_none.h"

chisel_sync_none::chisel_sync_none(int threadCount) : parent(threadCount) {
	// TODO Auto-generated constructor stub

}

chisel_sync_none::~chisel_sync_none() {
	// TODO Auto-generated destructor stub
}

void chisel_sync_none::worker_ready() {
}

void chisel_sync_none::worker_wait_work() {
}

void chisel_sync_none::worker_done() {
}

void chisel_sync_none::worker_wait_rest() {
}

void chisel_sync_none::master_wait_ready() {
}

void chisel_sync_none::master_work() {
}

void chisel_sync_none::master_wait_done() {
}

void chisel_sync_none::master_rest() {
}
