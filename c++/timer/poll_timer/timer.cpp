#include <timer.h>

using namespace libraries;
using namespace timer;

timer_object::timer_object(callback_t callback, void *args) 
{
	this->callback = callback;
	this->args = args;
}

void 
timer_object::exec_callback(void)
{
	this->callback(this->args);
}

libraries::timer::timer_t::timer_t(int polltime)
{
	this->poll_interval = polltime;
	pthread_mutex_init(&mutex, 0);
	pthread_create(&thread, 0, timer_t::start, this);
}

void *
libraries::timer::timer_t::start(void *args)
{
	libraries::timer::timer_t *thisp = (timer_t *)args;
	while (1) {
		/** start checking for timedout entries */
		thisp->poll();
		sleep(thisp->poll_interval);
	}

	return NULL;
}

void 
libraries::timer::timer_t::add(int timeout, callback_t cb, void *args) 
{
	timer_object obj(cb, args);

	pthread_mutex_lock(&mutex);
	timerList.insert(pair<int, timer_object>(timeout, obj));			
	pthread_mutex_unlock(&mutex);
}

void
libraries::timer::timer_t::poll(void) {
	int currtime = time(NULL);
	int timedout = 0;

	pthread_mutex_lock(&mutex);
	if (timerList.size() == 0) {
		pthread_mutex_unlock(&mutex);
		return;
	}
	timerList_t::iterator it = timerList.begin();
	for (; it != timerList.end(); it++) {
		if (it->first > currtime)
				break;

		/** Timed out; execute callbacks */
		(it->second).exec_callback();
		timedout = 1;
	}
		
	/** atleast one member timed out; clear the timed out entries */	
	if (timedout != 0)					
		timerList.erase(timerList.begin(), it);

	pthread_mutex_unlock(&mutex);
}

