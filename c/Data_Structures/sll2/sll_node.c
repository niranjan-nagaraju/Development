#include <sll_node.h>

/** Create a SLL node encapsulation of the "object" to be inserted into the SLL */
sll_node_t *
new_sll_node(void *object)
{
	sll_node_t *node = (sll_node_t *)malloc(sizeof(sll_node_t));
	if (!node)
		return NULL;

	node->object = object;
	node->next = NULL;

	return node;
}

/** Free the encapsulated SLL node and return its content */
void *
free_sll_node(sll_node_t *node)
{
	void *object;

	if (!node)
		return NULL;

	object = node->object;

	free(node);

	return object;
}


/** Retrieve the value encapsulated in the SLL node */
void *
value_sll_node(sll_node_t *node)
{
	if(!node)
		return NULL;

	return node->object;
}
