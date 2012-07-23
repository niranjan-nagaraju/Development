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


