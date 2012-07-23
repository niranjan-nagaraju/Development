#include <sll.h>
#include <stdio.h>

/** Print the SLL, what else? ;) */
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

