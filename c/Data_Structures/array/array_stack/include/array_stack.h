#ifndef __ARRAY_STACK_H__
#define __ARRAY_STACK_H__

#ifdef _MULTI_THREADED_
#include <pthread.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "boolean.h"

typedef struct array_stack_s {
	int tos; /** top of the stack */
	void **objects;
	int size;
	int curr_size;
#ifdef _MULTI_THREADED_
	pthread_mutex_t lock;
#endif
}array_stack_t;

/** The public interface */
int array_stack_init(array_stack_t *this, int size);
int array_stack_push(array_stack_t *this, void *object);
void *array_stack_pop(array_stack_t *this);
int array_stack_len(array_stack_t *this);
boolean array_stack_isFull(array_stack_t *this);
boolean array_stack_isEmpty(array_stack_t *this);
int array_stack_resize(array_stack_t *this, int newsize);
void *array_stack_peek(array_stack_t *this);
void *array_stack_peekAt(array_stack_t *this, int idx);
boolean array_stack_isThreadSafe(array_stack_t *this);
void array_stack_destroy(array_stack_t *this);
void array_stack_print(array_stack_t *this, void (*printfn)(void *object));

#endif /** __ARRAY_STACK_H__ */
