#include <sll.h>


void 
sll_init (sll_t *sll)
{
	if (!sll)
		return;

	sll->head = sll->tail = NULL;
	sll->_size = 0;
}

int 
sll_length (sll_t *sll)
{
	if (!sll)
		return -1;

	return sll->_size;
}

int 
sll_insert_at_front (sll_t *sll, void *data)
{
	sll_node_t *node = NULL;
	
	if (!sll)
		return -1;

	sll_node_create (data);

	if (!node)
		return -ENOMEM;

	/** Insert never fails :) */
	sll->_size++;

	node->next = sll->head;
	sll->head = node;

	/** SLL is empty */
	if ( sll->tail == NULL )
		sll->tail = node;

	return 0;
}

