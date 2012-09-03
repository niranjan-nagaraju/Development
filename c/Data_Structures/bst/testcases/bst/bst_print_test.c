#include <bst.h>
#include <assert.h>
#include <stdio.h>

int main(void)
{
	bst_t bst;
	
	bst_init(&bst);

	bst_insert(&bst, (void *)3, compareInts);

	bst_insert(&bst, (void *)2, compareInts);
	bst_insert(&bst, (void *)6, compareInts);

	bst_insert(&bst, (void *)1, compareInts);
	bst_insert(&bst, (void *)5, compareInts);
	bst_insert(&bst, (void *)7, compareInts);

	/** BST:
	 *	3
	 *	  6
	 *	    5
	 *	    7
	 *	  2
	 *	    1
	 */

	bst_print_preorder(&bst, printAsInt);
	bst_print_inorder(&bst, printAsInt);
	bst_print_postorder(&bst, printAsInt);

	/**
	 * 3 2 1 6 5 7 
	 * 1 2 3 5 6 7 
	 * 1 2 5 7 6 3 
	 */

	return 0;
}
