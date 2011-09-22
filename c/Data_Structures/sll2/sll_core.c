#include <sll.h>

void 
sll_init (sll_t *sll)
{
	if ( !sll )
		return;

	sll->head = sll->tail = NULL;
	sll->size = 0;
}


/** Completely destroy the SLL. If the objects were manually managed, also pass an appropriate routine to free the object storage */
void 
sll_destroy (sll_t *this, void (*deallocate)(void *object))
{
	sll_node_t *trav, *prev;

	SLL_LOCK(this);

	trav = this->head;

	while (trav) {
		prev = trav;
		trav = trav->next;

		/** If the object has a custom allocator, use its deallocate routine */
		if (deallocate)
			deallocate(prev->object);

		/** De-allocate the node itself */
		free_sll_node(prev);
	}

	/** Just playing safe */
	this->_size = 0;
	this->head = this->tail = NULL;

	SLL_UNLOCK(this);
}

int
sll_isThreadSafe (sll_t *this)
{
#ifdef _MULTI_THREADED_
	return 1;
#else
	return 0;
#endif
}
