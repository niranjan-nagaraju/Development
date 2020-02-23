#ifndef __THREADED_TIMER_H__
#define __THREADED_TIMER_H__

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

typedef void(*callback_t)(void *);

typedef struct threaded_timer_s {
	unsigned int interval;
	unsigned int time_expires;	/** Absolute time at which timer expires */
	unsigned int recurrent;
	unsigned int max_times;		/** 0 => Repeat forever */
	unsigned int curr_times;	/** Number of times, the timer has expired thus far */
	unsigned int poll_interval;

	callback_t callback;
	void *args;

	pthread_mutex_t mutex;
	pthread_t thread;
} threaded_timer_t;

threaded_timer_t *set_timer(unsigned int interval, unsigned int recurrent, unsigned int max_times, unsigned int poll_interval, callback_t callback, void *args);
int reset_timer(threaded_timer_t *timer, int interval, unsigned int recurrent, unsigned int max_times, unsigned int poll_interval, callback_t callback, void *args);
void delete_timer(threaded_timer_t *timer);

void start_timer(threaded_timer_t *timer);

void *poll_timer (void *t);
#endif
