#include <sll.h>
#include <stdio.h>

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

/** Insert at the beginning of the SLL */
int 
sll_insert_at_front (sll_t *sll, void *data)
{
	sll_node_t *node = NULL;
	
	if (!sll)
		return -1;

	node = sll_node_create (data);

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


int 
sll_insert_at_end (sll_t *sll, void *data)
{
	sll_node_t *node = NULL;

	if (!sll)
		return -1;

	node = sll_node_create (data);
	if (!node)
		return -ENOMEM;

	/** SLL is empty */
	if (! sll->tail) {
		sll->tail = sll->head = node;
		return 0;
	}

	return 0;
}


int 
sll_insert_at_position (sll_t *sll, void *data, int pos)
{
	return 0;
}

void *
sll_delete_from_front (sll_t *sll)
{
	return NULL;
}


void *
sll_delete_from_end (sll_t *sll)
{
	return NULL;
}


void *
sll_delete_from_position (sll_t *sll, int pos)
{
	return NULL;
}

void 
sll_print (sll_t *sll, void(*printfn)(void *))
{
	sll_node_t *tmp;

	if (!sll)
		return;

	/** print size */
	printf("[%d]: ", sll->_size);

	tmp = sll->head;

	while ( tmp != NULL ) {
		sll_node_print(tmp, printfn);
		tmp = tmp->next;
		printf(" ");
	}

	printf("\n");
}

