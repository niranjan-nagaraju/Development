#include <sll_node.h>
#include <stdio.h>

sll_node_t *
sll_node_create (void *data)
{
	sll_node_t *tmp = NULL;

	tmp = (sll_node_t *) malloc (sizeof(sll_node_t));

	if (!tmp)
		return NULL;

	tmp->data = data;
	tmp->next = NULL;

	printf ("%p\n", tmp);
	return tmp;
}

void *
sll_node_delete (sll_node_t *node)
{
	void *data = NULL;

	if (!node)
		return;

	data = node->data;
	
	free(node);

	return data;
}


void 
sll_node_print (sll_node_t *node, void (*printfn)(void *))
{
	printfn(node->data);
}

