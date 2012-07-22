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

