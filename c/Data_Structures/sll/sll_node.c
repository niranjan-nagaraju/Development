#include <sll_node.h>

/** 
 * SLL Node helper functions
 * No Error checking is performed here
 *  for 
 *   a) The functions are not expected to be called directly
 *   b) SLL is assumed to check for the boundary conditions, et, al.
 */


/** Create an SLL node; Initialize it with given data */
sll_node_t *
sll_node_create (void *data)
{
	sll_node_t *tmp = NULL;

	tmp = (sll_node_t *) malloc (sizeof(sll_node_t));

	if (!tmp)
		return NULL;

	tmp->data = data;
	tmp->next = NULL;

	return tmp;
}

/** Deallocate specifed SLL node, return data */
/** TODO: Extend to deallocate 'data' as well if deallocation method 
 *		for 'data' is specified
 */
void *
sll_node_delete (sll_node_t *node)
{
	void *data = NULL;

	data = node->data;
	
	free(node);

	return data;
}

/** Print the data contained in an SLL; SLL/User provides how to print the data */
void 
sll_node_print (sll_node_t *node, void (*printfn)(void *))
{
	printfn(node->data);
}

