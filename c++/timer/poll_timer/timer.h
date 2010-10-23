#ifndef __TIMER_H__

#define __TIMER_H__

#include <iostream>
#include <string>
#include <map>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>

using namespace std;

namespace libraries 
{
	namespace timer 
	{
		typedef void(*callback_t)(void *);

		class timer_object
		{
			private:
				void *args;
				callback_t callback;

			public:
				timer_object(callback_t callback, void *args);
				void exec_callback(void);
		};
		
		typedef multimap<int, timer_object> timerList_t;
		
		class timer_t 
		{
			private:
				int poll_interval;
				timerList_t timerList;
				pthread_t thread;
				pthread_mutex_t mutex;	/** Protect the timer list */

			public:
				timer_t (int polltime = 1);
				void add (int timeout, void (*cb)(void *args), void *args=NULL);
				void poll(void);
				static void *start(void *args);
		};
	}
}
#endif
