#include "array_queue.h"

#define LOCK_QUEUE() \
	if (this->threadsafe)	\
		pthread_mutex_lock(&(this->lock));

#define UNLOCK_QUEUE() \
	if (this->threadsafe)	\
		pthread_mutex_unlock(&(this->lock));

/** 
 * Initialize this array_queue 
 * Call before any of the array_queue operations r initiated
 */
int initArrayQueue(array_queue_t *this, int size)
{
	this->objects = calloc(size, sizeof(void *));
	if ( !this->objects )
		return -1;

	this->head = this-> tail = -1;

	this->size = size;
	this->curr_size = 0;
	this->threadsafe = 0;
	pthread_mutex_init(&(this->lock), NULL);

	this->enqueue = addToArrayQueue;
	this->dequeue = removeFromArrayQueue;
	this->resize = resizeArrayQueue;
	this->length = lengthOfArrayQueue;
	this->peek = peekArrayQueue;
	this->erase = eraseArrayQueue;
	this->isThreadSafe = isThreadSafeArrayQueue;
	this->setThreadSafe = setThreadSafeArrayQueue;
	this->print = printArrayQueue;

	return 0;
}

int addToArrayQueue(array_queue_t *this, void *object) 
{
	LOCK_QUEUE();
	(this->tail)++;
	if (this->tail > this->size) {
		UNLOCK_QUEUE();
		return -1;
	}

	this->objects[this->tail] = object;
	if (this->tail == 0)	/** First element in the queue */
		(this->head)++;

	(this->curr_size)++;

	UNLOCK_QUEUE();
	return 0;
}

void *removeFromArrayQueue(array_queue_t *this)
{
	void *object;
	
	LOCK_QUEUE();
	if (this->head == -1) {
		UNLOCK_QUEUE();
		return NULL;
	}

	object = this->objects[(this->head)];
	(this->head)++;
	
	(this->curr_size)--;

	if(this->head == this->size)
		this->head = this->tail = -1;

	UNLOCK_QUEUE();
	return object;
}

int resizeArrayQueue(array_queue_t *this, int newsize)
{
	void **newObjects = NULL;
	int i;

	LOCK_QUEUE();
	if (newsize < this->size) {
		UNLOCK_QUEUE();
		return 0;
	}

	newObjects = calloc(newsize, sizeof(void *));
	if (! newObjects)
		return -1;

	LOCK_QUEUE();
	for(i=0; i<(this->curr_size); i++)
		newObjects[i] = this->objects[i];

	free(this->objects);

	this->objects = newObjects;

	UNLOCK_QUEUE();
	return 0;
}

int lengthOfArrayQueue(array_queue_t *this)
{
	int size;

	LOCK_QUEUE();
	size = this->curr_size;
	UNLOCK_QUEUE();

	return size;
}

void *peekArrayQueue(array_queue_t *this)
{
	void *object;

	LOCK_QUEUE();
	if (this->head == -1) {
		UNLOCK_QUEUE();
		return NULL;
	}

	object = this->objects[this->head];
	UNLOCK_QUEUE();

	return object;
}

void eraseArrayQueue(array_queue_t *this)
{
	LOCK_QUEUE();
	free(this->objects);
	this->head = this->tail = -1;
	this->curr_size = this->size = 0;
	UNLOCK_QUEUE();
}

int isThreadSafeArrayQueue(array_queue_t *this)
{
	return this->threadsafe;
}

void setThreadSafeArrayQueue(array_queue_t *this)
{
	this->threadsafe = 1;
}

void printArrayQueue(array_queue_t *this, void (*printfn)(void *object))
{
	int i;

	LOCK_QUEUE();
	printf("Q(sz: %d, cap: %d, head: %d, tail: %d): \n", this->curr_size, this->size, this->head, this->tail);
	for (i=this->head; i<(this->tail); i++) {
		printfn(this->objects[i]);
		printf(" ");
	}

	printfn(this->objects[i]);
	UNLOCK_QUEUE();

	printf("\n");
}
