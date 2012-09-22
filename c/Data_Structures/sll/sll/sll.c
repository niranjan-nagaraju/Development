#include <sll.h>


/** Initialize Singly Linked List */
void 
sll_init (sll_t *sll)
{
	if (!sll)
		return;

	sll->head = sll->tail = NULL;
	sll->_size = 0;
}


/** Return size of the SLL */
int 
sll_length (sll_t *sll)
{
	if (!sll)
		return -1;

	return sll->_size;
}


/** Destroy SLL completely */
void
sll_destroy (sll_t *sll, deallocatorfn deallocate)
{
	sll_node_t *tmp, *trav;

	if (!sll)
		return;

	/** We don't use tail and size, might as well reset them right here */
	sll->tail = NULL;
	sll->_size = 0;

	trav = sll->head;
	while ( trav != NULL )
	{
		tmp = trav;
		trav = trav->next;

		/** call user supplied memory deallocator */
		if (deallocate)
			deallocate (tmp->data);
	}

	sll->head = NULL;
}
