#include <queue.h>

void
queue_init(queue_t *this)
{
	this->list = SLL_INITIALIZER;
}


int
enqueue(queue_t *this, void *object)
{
	return sll_insert_at_end(&this->list, object);
}

void *
dequeue(queue_t *this)
{
	return sll_remove_at_front(&this->list);
}


int
queue_length(queue_t *this)
{
	return sll_length(&this->list);
}

boolean
queue_isFull(queue_t *this)
{
	return FALSE;
}

boolean
queue_isEmpty(queue_t *this)
{
	return (sll_length(&this->list) == 0);
}

void *
queue_peekFront(queue_t *this)
{
	//return this->
	return NULL;
}

void *
queue_peekRear(queue_t *this)
{
	return NULL;
}

boolean
queue_isThreadSafe(queue_t *this)
{
	return FALSE;
}

void
queue_destroy(queue_t *this)
{
	/** TODO: Include deallocator later */
	sll_destroy(&this->list, 0);
}

void
queue_print(queue_t *this, void (*printfn)(void *object))
{
	sll_print(&this->list, printfn);
}


