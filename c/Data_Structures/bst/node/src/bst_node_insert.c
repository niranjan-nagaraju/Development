#include <bst_node.h>

/**
 * Insert to the left/right of a node
 * No error checking here,
 * BST is assumed to do it before calling these
 * helper functions
 */


/** Insert specified data to the left of the parent node */
int 
bst_node_insert_to_left (bst_node_t *parent, void *data)
{
	bst_node_t *node = bst_node_create(data);

	if (!node)
		return -ENOMEM;

	parent->left = node;

	return 0;
}


/** Insert specified node to the left of the parent node */
int 
bst_node_insert_node_to_left (bst_node_t *parent, bst_node_t *node)
{
	parent->left = node;

	return 0;
}


/** Insert specified data to the right of the parent node */
int 
bst_node_insert_to_right (bst_node_t *parent, void *data)
{
	bst_node_t *node = bst_node_create(data);

	if (!node)
		return -ENOMEM;

	parent->right = node;

	return 0;
}


/** Insert specified node to the left of the parent node */
int 
bst_node_insert_node_to_right (bst_node_t *parent, bst_node_t *node)
{
	parent->right = node;

	return 0;
}
