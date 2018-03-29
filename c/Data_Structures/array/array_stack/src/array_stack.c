#include "array_stack.h"
#include "array_stack_internal.h"

/** 
 * Initialize this array_stack 
 * Call before any of the array_stack operations r initiated
 */
int
array_stack_init(array_stack_t *this, int size)
{
	this->objects = calloc(size, sizeof(void *));
	if ( !this->objects )
		return -ENOMEM;

	this->tos = -1;

	this->size = size;
	this->curr_size = 0;

	LOCK_INIT(this);

	return 0;
}



boolean 
array_stack_isFull(array_stack_t *this)
{
	boolean rc;

	if (!this)
		return FALSE;

	LOCK_STACK(this);
	rc = _array_stack_isFull(this);
	UNLOCK_STACK(this);

	return rc;
}


boolean 
array_stack_isEmpty(array_stack_t *this)
{
	boolean rc;

	if (!this)
		return FALSE;

	LOCK_STACK(this);
	rc = _array_stack_isEmpty(this);
	UNLOCK_STACK(this);

	return rc;
}


int
array_stack_push(array_stack_t *this, void *object) 
{
	if (!this)
		return -EINVAL;

	LOCK_STACK(this);

	/** Stack overflow */
	if (_array_stack_isFull(this)) {
		UNLOCK_STACK(this);
		return -EINVAL;
	}

	(this->tos)++;
	this->objects[this->tos] = object;

	(this->curr_size)++;

	UNLOCK_STACK(this);
	return 0;
}

void *
array_stack_pop(array_stack_t *this)
{
	void *object;
	
	if (!this)
		return NULL;

	LOCK_STACK(this);

	/** Queue underflow */
	if (_array_stack_isEmpty(this)) {
		UNLOCK_STACK(this);
		return NULL;
	}

	object = this->objects[(this->tos)];

	(this->tos)--;
	
	(this->curr_size)--;

	UNLOCK_STACK(this);
	return object;
}

int
array_stack_resize(array_stack_t *this, int newsize)
{
	void **newObjects = NULL;
	int i;

	if (!this)
		return -EINVAL;

	LOCK_STACK(this);

	/** No need to allocate more, we are already over and above the needed size */
	if (newsize <= this->size) {
		UNLOCK_STACK(this);
		return 0;
	}

	newObjects = calloc(newsize, sizeof(void *));
	if (! newObjects)
		return -ENOMEM;

	LOCK_STACK(this);

	/** Copy objects from old allocation */
	for(i=0; i<(this->curr_size); i++)
		newObjects[i] = this->objects[i];

	free(this->objects);

	this->objects = newObjects;

	UNLOCK_STACK(this);
	return 0;
}

int
array_stack_len(array_stack_t *this)
{
	int size;

	if (!this)
		return 0;

	LOCK_STACK(this);
	size = this->curr_size;
	UNLOCK_STACK(this);

	return size;
}

void *
array_stack_peek(array_stack_t *this)
{
	void *object;

	if (array_stack_isEmpty(this)) 
		return NULL;

	LOCK_STACK(this);
	object = this->objects[this->tos];
	UNLOCK_STACK(this);

	return object;
}


void *
array_stack_peekAt(array_stack_t *this, int idx)
{
	void *object;

	if (array_stack_isEmpty(this))
		return NULL;

	LOCK_STACK(this);

	/** We don't have enough elements in the stack to go until 'idx' */
	if (idx >= this->curr_size) {
		UNLOCK_STACK(this);
		return NULL;
	}
	object = this->objects[this->tos-idx];
	UNLOCK_STACK(this);

	return object;
}


void
array_stack_destroy(array_stack_t *this)
{
	LOCK_STACK(this);
	free(this->objects);
	this->tos = -1;
	this->curr_size = this->size = 0;
	UNLOCK_STACK(this);
}

boolean
array_stack_isThreadSafe(array_stack_t *this)
{
#ifdef _MULTI_THREADED_
	return TRUE;
#else
	return FALSE;
#endif
}

void
array_stack_print(array_stack_t *this, void (*printfn)(void *object))
{
	int i;

	LOCK_STACK(this);
	printf("Stack(sz: %d, cap: %d, tos: %d): \n", 
			this->curr_size, this->size, this->tos);

	if (_array_stack_isEmpty(this)) {
		UNLOCK_STACK(this);
		return;
	}

	for ( i=0; i< this->curr_size; i++) {
		int idx = this->tos - i;
		printfn(this->objects[idx]);
		printf(" ");

	}

	UNLOCK_STACK(this);

	printf("\n");
}
