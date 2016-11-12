#ifndef __ARRAY_QUEUE_INTERNAL_H__
#define __ARRAY_QUEUE_INTERNAL_H__


#ifdef _MULTI_THREADED_	
#define LOCK_INIT(this) pthread_mutex_init(&((this)->lock), NULL)
#define LOCK_QUEUE(this) pthread_mutex_lock(&((this)->lock))
#define UNLOCK_QUEUE(this) pthread_mutex_unlock(&((this)->lock))
#else
#define LOCK_INIT(this) 
#define LOCK_QUEUE(this)
#define UNLOCK_QUEUE(this)
#endif

/** Unsafe versions, Not MT or NULL-safe */
static boolean 
_arrayQ_isCircular(array_queue_t *this)
{
	return this->circular;
}

static boolean 
_arrayQ_isFull(array_queue_t *this)
{
	if (this->circular)
		return (this->curr_size == this->size);
	else 
		return (this->tail == (this->size-1));
}


static boolean 
_arrayQ_isEmpty(array_queue_t *this)
{
	return (this->curr_size == 0);
}

#endif /** __ARRAY_QUEUE_INTERNAL_H__ */
