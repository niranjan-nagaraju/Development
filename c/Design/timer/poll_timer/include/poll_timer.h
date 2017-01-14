#ifndef __POLL_TIMER_H__
#define __POLL_TIMER_H__

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

typedef void(*callback_t)(void *);

typedef struct poll_timer_s {
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
} poll_timer_t;

poll_timer_t *set_timer(unsigned int interval, unsigned int recurrent, unsigned int max_times, unsigned int poll_interval, callback_t callback, void *args);
int reset_timer(poll_timer_t *timer, int interval, unsigned int recurrent, unsigned int max_times, unsigned int poll_interval, callback_t callback, void *args);
void delete_timer(poll_timer_t *timer);

void start_timer(poll_timer_t *timer);

void *poll_timer (void *t);
#endif
