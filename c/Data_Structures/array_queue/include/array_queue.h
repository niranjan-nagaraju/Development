#ifndef __ARRAY_QUEUE_H__
#define __ARRAY_QUEUE_H__

#ifdef _MULTI_THREADED_
#include <pthread.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "boolean.h"

typedef struct array_queue_s {
	int head; /** front of the queue */
	int tail; /** rear of the queue */
	void **objects;
	int size;
	int curr_size;
	/** circular Queues help store 'size' number of elements at all times, default True */
	boolean circular; 
#ifdef _MULTI_THREADED_
	pthread_mutex_t lock;
#endif
}array_queue_t;

/** The public interface */
int arrayQ_init(array_queue_t *this, int size);
void arrayQ_setCircular(array_queue_t *this);
int arrayQ_enqueue(array_queue_t *this, void *object);
void *arrayQ_dequeue(array_queue_t *this);
int arrayQ_len(array_queue_t *this);
boolean arrayQ_isCircular(array_queue_t *this);
boolean arrayQ_isFull(array_queue_t *this);
boolean arrayQ_isEmpty(array_queue_t *this);
int arrayQ_resize(array_queue_t *this, int newsize);
void *arrayQ_peekFront(array_queue_t *this);
void *arrayQ_peekRear(array_queue_t *this);
void *arrayQ_peekAt(array_queue_t *this, int idx);
boolean arrayQ_isThreadSafe(array_queue_t *this);
void arrayQ_destroy(array_queue_t *this);
void arrayQ_print(array_queue_t *this, void (*printfn)(void *object));

#endif /** __ARRAY_QUEUE_H__ */
