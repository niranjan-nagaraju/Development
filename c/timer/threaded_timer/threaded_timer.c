#include <threaded_timer.h>

void *
poll_timer (void *t)
{
	timer_t *timer = (timer_t *)t;
	int currtime;

	while (1) {
		currtime = time(NULL);

		/** Guard if Delete Timer called from another thread */
		if (!timer)
			return NULL;

		pthread_mutex_lock(&(timer->mutex));

		if (currtime >= timer->time_expires) {
			timer->callback(timer->args);

			timer->time_expires += timer->interval;

			(timer->curr_times)++;
		}

		if ((!timer->recurrent) || (timer->max_times && (timer->curr_times == timer->max_times))) {
			pthread_mutex_unlock(&(timer->mutex));
			return NULL;
		}

		pthread_mutex_unlock(&(timer->mutex));
		
		sleep(timer->poll_interval);
	}

	delete_timer(timer);
	return NULL;
}

timer_t *
set_timer(unsigned int interval, unsigned int recurrent, unsigned int max_times, unsigned int poll_interval, callback_t callback, void *args)
{
	timer_t *timer = (timer_t *) malloc(sizeof(timer_t));
	int currtime = time(NULL);

	if (!timer)
		return NULL;

	if (!interval || !callback) {
		free(timer);
		return NULL;
	}

	pthread_mutex_init(&(timer->mutex), 0);

	timer->interval = interval;
	timer->time_expires = currtime + interval;
	timer->recurrent = recurrent;

	if (!poll_interval)
		poll_interval = 1;

	timer->max_times = max_times;
	timer->poll_interval = poll_interval;
	timer->callback = callback;
	timer->args = args;

	return timer;
}

int
reset_timer(timer_t *timer, int interval, unsigned int recurrent, unsigned int max_times, unsigned int poll_interval, callback_t callback, void *args)
{
	int currtime = time(NULL);

	pthread_mutex_lock(&(timer->mutex));
	if (!timer) {
		pthread_mutex_unlock(&(timer->mutex));
		return -1;
	}

	pthread_mutex_lock(&(timer->mutex));
	if (interval)
		timer->interval = interval;

	timer->time_expires = currtime + interval;
	timer->curr_times = 0;

	/** Recurrent setting has changed */
	if (timer->recurrent != recurrent)
		timer->recurrent = recurrent;

	if (timer->max_times != max_times)
		timer->max_times = max_times;

	if (timer->poll_interval)
		timer->poll_interval = poll_interval;

	if (timer->callback)
		timer->callback = callback;

	if (timer->args)
		timer->args = args;

	pthread_mutex_unlock(&(timer->mutex));

	return 0;
}

void 
delete_timer(timer_t *timer)
{
	if (timer) {
		pthread_mutex_lock(&(timer->mutex));
		free(timer);
		pthread_mutex_unlock(&(timer->mutex));
	}
}

void 
start_timer(timer_t *timer)
{
	pthread_create(&(timer->thread), 0, poll_timer, timer);
}
