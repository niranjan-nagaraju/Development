#include <bst.h>
#include <assert.h>
#include <stdio.h>

int main(void)
{
	bst_t bst;
	bst_node_t *node, *tmp;
	
	bst_init(&bst);

	bst_insert(&bst, (void *)3, compareInts);

	bst_insert(&bst, (void *)2, compareInts);
	bst_insert(&bst, (void *)6, compareInts);

	bst_insert(&bst, (void *)1, compareInts);
	bst_insert(&bst, (void *)5, compareInts);
	bst_insert(&bst, (void *)7, compareInts);

	/**
	 * BST:
	 *	       7
	 *	    6->
	 *	       5 
	 *   3->
	 *         *
	 *      2->
	 *         1
	 */      

	assert(bst._num_nodes == 6);

	/** Level 0 */
	node = bst.root;
	assert((int)(node->data) == 3);

	/** Level 1 */
	assert(node->right != NULL);
	assert((int)(node->right->data) == 6);

	assert(node->left != NULL);
	assert((int)(node->left->data) == 2);

	/* Level 2 */
	tmp = node;
	node = node->left;

	/** follow subtree under (2) */
	assert((int)(node->data) == 2);
	assert(node->right == NULL);

	assert(node->left != NULL);
	assert((int)(node->left->data) == 1);

	/** follow subtree under (6) */
	node = tmp->right;
	assert((int)(node->data) == 6);
	assert(node->left != NULL);
	assert((int)(node->left->data) == 5);

	assert(node->right != NULL);
	assert((int)(node->right->data) == 7);

	printf("BST Insert tests successful\n");

	return 0;
}
