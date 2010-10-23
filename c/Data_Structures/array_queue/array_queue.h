#ifndef __ARRAY_QUEUE_H__
#define __ARRAY_QUEUE_H__

#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct array_queue{
	int head, tail;
	void **objects;
	int size;
	int curr_size;

	int threadsafe;
	pthread_mutex_t lock;

	int (*enqueue)(struct array_queue *this, void *object);
	void *(*dequeue)(struct array_queue *this);
	int (*resize)(struct array_queue *this, int newsize);
	int (*length)(struct array_queue *this);
	void *(*peek)(struct array_queue *this);
	void (*erase)(struct array_queue *this);
	int (*isThreadSafe) (struct array_queue *this);
	void (*setThreadSafe) (struct array_queue *this);
	void (*print) (struct array_queue *this, void (*printfn)(void *object));
}array_queue_t;

int initArrayQueue(array_queue_t *this, int size);
int addToArrayQueue(array_queue_t *this, void *object);
void *removeFromArrayQueue(array_queue_t *this);
int resizeArrayQueue(array_queue_t *this, int newsize);
int lengthOfArrayQueue(array_queue_t *this);
void *peekArrayQueue(array_queue_t *this);
void eraseArrayQueue(array_queue_t *this);
int isThreadSafeArrayQueue(array_queue_t *this);
void setThreadSafeArrayQueue(array_queue_t *this);
void printArrayQueue(struct array_queue *this, void (*printfn)(void *object));

#endif /** __ARRAY_QUEUE_H__ */
