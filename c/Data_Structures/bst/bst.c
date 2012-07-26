#include <bst.h>

/** Initialize a binary search tree */
void 
bst_init (bst_t *tree)
{
	tree->root = NULL;
	tree->_num_nodes = 0;
}


/** Return number of nodes in the BST */
int 
bst_size (bst_t *tree)
{
	if (!tree)
		return -1;

	return tree->_num_nodes;
}


/** Return width - Maximum number of nodes at any level, of the BST */
int 
bst_width (bst_t *tree)
{
	if (!tree)
		return -1;

	return 0;
}


/** Return depth - Maximum number of nodes in any path from root to a leaf node, of the BST */
int 
bst_depth (bst_t *tree)
{
	if (!tree)
		return -1;

	return 0;
}
