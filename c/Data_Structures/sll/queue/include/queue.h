#ifndef __QUEUE_H__
#define __QUEUE_H__

#include<sll.h>

typedef struct sll_queue_s {
	sll_t list;
} queue_t;

void queue_init(queue_t *this);
int enqueue(queue_t *this, void *object);
void *dequeue(queue_t *this);

int queue_length(queue_t *this);
boolean queue_isFull(queue_t *this);
boolean queue_isEmpty(queue_t *this);
void *queue_peekFront(queue_t *this);
void *queue_peekRear(queue_t *this);
boolean queue_isThreadSafe(queue_t *this);
void queue_destroy(queue_t *this);
void queue_print(queue_t *this, void (*printfn)(void *object));

#endif
