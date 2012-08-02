#include <bst.h>


/** Insert new node under the parent node recursively */
static int
bst_insert_node_helper_R(bst_node_t *parent, bst_node_t *node, comparefn compare)
{
	/** node's data > parent's data; Insert to the right */
	if (compare(node->data, parent->data) > 0) {
		/** 
		 * parent's node has an empty slot to the right; 
		 * Place new node there and return
		 */
		if (! bst_node_has_right(parent) ) {
			return bst_node_insert_node_to_right (parent, node);
		}

		/** else 'try' to insert under parent node */
		return bst_insert_node_helper_R (parent->right, node, compare);
	} else { /** node's data <= parent's data; Insert to the left */
		/** 
		 * parent's node has an empty slot to the left; 
		 * Place new node there and return
		 */
		if (! bst_node_has_left(parent) ) {
			return bst_node_insert_node_to_left (parent, node);
		}

		/** else 'try' to insert under parent node */
		return bst_insert_node_helper_R (parent->left, node, compare);
	}

	return 0;
}

static int
bst_insert_node (bst_t *tree, bst_node_t *node, comparefn compare)
{
	/** Insert never fails */
	tree->_num_nodes++;

	if (! tree->root) {
		tree->root = node;

		return 0;
	}

	return bst_insert_node_helper_R(tree->root, node, compare);
}

int
bst_insert(bst_t *tree, void *data, comparefn compare)
{
	bst_node_t *node = NULL;
	
	if (! tree)
		return -1;

	node = bst_node_create(data);

	if (!node)
		return -ENOMEM;

	return bst_insert_node (tree, node, compare);
}
