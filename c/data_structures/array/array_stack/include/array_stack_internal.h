#ifndef __ARRAY_STACK_INTERNAL_H__
#define __ARRAY_STACK_INTERNAL_H__


#ifdef _MULTI_THREADED_	
#define LOCK_INIT(this) pthread_mutex_init(&((this)->lock), NULL)
#define LOCK_STACK(this) pthread_mutex_lock(&((this)->lock))
#define UNLOCK_STACK(this) pthread_mutex_unlock(&((this)->lock))
#else
#define LOCK_INIT(this) 
#define LOCK_STACK(this)
#define UNLOCK_STACK(this)
#endif

/** Unsafe versions, Not MT or NULL-safe */

static boolean 
_array_stack_isFull(array_stack_t *this)
{
	return (this->curr_size == this->size);
}


static boolean 
_array_stack_isEmpty(array_stack_t *this)
{
	return (this->curr_size == 0);
}

#endif /** __ARRAY_QUEUE_INTERNAL_H__ */
