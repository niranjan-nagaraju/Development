#include <bst_node.h>

/** 
 * BST Node helper functions
 * No Error checking is performed here
 *  for 
 *   a) The functions are not expected to be called directly
 *   b) BST is assumed to check for the boundary conditions, et, al.
 */


/** Create a BST node; Initialize it with given data */
bst_node_t *
bst_node_create (void *data)
{
	bst_node_t *tmp = NULL;

	tmp = (bst_node_t *) malloc (sizeof(bst_node_t));

	if (!tmp)
		return NULL;

	tmp->data = data;
	tmp->left = NULL;
	tmp->right = NULL;

	return tmp;
}


/** Deallocate specifed BST node, return data */
/** TODO: Extend to deallocate 'data' as well if deallocation method 
 *		for 'data' is specified
 */
void *
bst_node_delete (bst_node_t *node)
{
	void *data = NULL;

	data = node->data;
	
	free(node);

	return data;
}


/** Print the data contained in an BST; BST/User provides how to print the data */
void 
bst_node_print (bst_node_t *node, void (*printfn)(void *))
{
	printfn(node->data);
}
