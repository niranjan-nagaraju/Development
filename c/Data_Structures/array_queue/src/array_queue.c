#include "array_queue.h"
#include "array_queue_internal.h"

/** 
 * Initialize this array_queue 
 * Call before any of the array_queue operations r initiated
 */
int
arrayQ_init(array_queue_t *this, int size)
{
	this->objects = calloc(size, sizeof(void *));
	if ( !this->objects )
		return -ENOMEM;

	this->head = this-> tail = -1;

	/** Use linear/non-circular for now */
	this->circular = FALSE;
	this->size = size;
	this->curr_size = 0;

	LOCK_INIT(this);

	return 0;
}


boolean 
arrayQ_isCircular(array_queue_t *this)
{
	boolean rc;

	if (!this)
		return FALSE;

	LOCK_QUEUE(this);
	rc = _arrayQ_isCircular(this);
	UNLOCK_QUEUE(this);

	return rc;
}

boolean 
arrayQ_isFull(array_queue_t *this)
{
	boolean rc;

	if (!this)
		return FALSE;

	LOCK_QUEUE(this);
	rc = _arrayQ_isFull(this);
	UNLOCK_QUEUE(this);

	return rc;
}


boolean 
arrayQ_isEmpty(array_queue_t *this)
{
	boolean rc;

	if (!this)
		return FALSE;

	LOCK_QUEUE(this);
	rc = _arrayQ_isEmpty(this);
	UNLOCK_QUEUE(this);

	return rc;
}


int
arrayQ_enqueue(array_queue_t *this, void *object) 
{
	if (!this)
		return -EINVAL;

	LOCK_QUEUE(this);

	/** Queue overflow */
	if (this->tail == this->size-1) {
		UNLOCK_QUEUE(this);
		return -EINVAL;
	}

	(this->tail)++;
	this->objects[this->tail] = object;
	if (this->head == -1)	/** First element in the queue */
		this->head = 0;

	(this->curr_size)++;

	UNLOCK_QUEUE(this);
	return 0;
}

void *
arrayQ_dequeue(array_queue_t *this)
{
	void *object;
	
	if (!this)
		return NULL;

	LOCK_QUEUE(this);

	/** Queue underflow */
	if (this->head == -1) {
		UNLOCK_QUEUE(this);
		return NULL;
	}

	object = this->objects[(this->head)];
	(this->head)++;
	
	(this->curr_size)--;

	/** This was the last element in the queue, Queue is now empty */
	if(this->head == this->size)
		this->head = this->tail = -1;

	UNLOCK_QUEUE(this);
	return object;
}

int
arrayQ_resize(array_queue_t *this, int newsize)
{
	void **newObjects = NULL;
	int i;

	if (!this)
		return -EINVAL;

	LOCK_QUEUE(this);

	/** No need to allocate more, we are already over and above the needed size */
	if (newsize <= this->size) {
		UNLOCK_QUEUE(this);
		return 0;
	}

	newObjects = calloc(newsize, sizeof(void *));
	if (! newObjects)
		return -ENOMEM;

	LOCK_QUEUE(this);
	for(i=0; i<(this->curr_size); i++)
		newObjects[i] = this->objects[i];

	free(this->objects);

	this->objects = newObjects;

	UNLOCK_QUEUE(this);
	return 0;
}

int
arrayQ_len(array_queue_t *this)
{
	int size;

	if (!this)
		return 0;

	LOCK_QUEUE(this);
	size = this->curr_size;
	UNLOCK_QUEUE(this);

	return size;
}

void *
arrayQ_peekFront(array_queue_t *this)
{
	void *object;

	if (arrayQ_isEmpty(this)) 
		return NULL;

	LOCK_QUEUE(this);
	object = this->objects[this->head];
	UNLOCK_QUEUE(this);

	return object;
}

void *
arrayQ_peekRear(array_queue_t *this)
{
	void *object;

	if (arrayQ_isEmpty(this)) 
		return NULL;

	LOCK_QUEUE(this);
	object = this->objects[this->tail];
	UNLOCK_QUEUE(this);

	return object;
}

void *
arrayQ_peekAt(array_queue_t *this, int idx)
{
	void *object;

	if (arrayQ_isEmpty(this))
		return NULL;

	LOCK_QUEUE(this);

	/** We don't have enough elements in the queue to go until 'idx' */
	if (idx >= this->curr_size) {
		UNLOCK_QUEUE(this);
		return NULL;
	}
	object = this->objects[this->head+idx];
	UNLOCK_QUEUE(this);

	return object;
}
void
arrayQ_destroy(array_queue_t *this)
{
	LOCK_QUEUE(this);
	free(this->objects);
	this->head = this->tail = -1;
	this->curr_size = this->size = 0;
	UNLOCK_QUEUE(this);
}

boolean
arrayQ_isThreadSafe(array_queue_t *this)
{
#ifdef _MULTI_THREADED_
	return TRUE;
#else
	return FALSE;
#endif
}

void
arrayQ_print(array_queue_t *this, void (*printfn)(void *object))
{
	int i;

	LOCK_QUEUE(this);
	printf("Q(sz: %d, cap: %d, head: %d, tail: %d): \n", this->curr_size, 
			this->size, this->head, this->tail);
	for (i=this->head; i<=(this->tail); i++) {
		printfn(this->objects[i]);
		printf(" ");
	}

	UNLOCK_QUEUE(this);

	printf("\n");
}
