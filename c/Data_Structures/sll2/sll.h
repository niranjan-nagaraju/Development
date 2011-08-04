#ifndef _SLL_CORE_H_
#define _SLL_CORE_H_

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <errno.h>

#include <common.h>

#include <sll_node.h>

#ifdef _MULTI_THREADED_
	#define	SLL_LOCK_INIT(sll)	pthread_mutex_init(&(sll->lock), NULL)
	#define SLL_LOCK(sll)		pthread_mutex_lock(&(sll->lock))
	#define SLL_UNLOCK(sll)		pthread_mutex_unlock(&(sll->lock))
#else
	#define	SLL_LOCK_INIT(sll) 
	#define SLL_LOCK(sll)
	#define SLL_UNLOCK(sll)
#endif


typedef struct sll_s {
	sll_node_t *head;
	sll_node_t *tail;

	int size;
} sll_t;

void sll_init (sll_t  *sll);
void sll_destroy (sll_t *this, void (*deallocate)(void *object));
int sll_isThreadSafe (sll_t *this); 

#endif
