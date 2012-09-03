#include <bst.h>


/**
 * Print routines for the binary search tree
 */

/** preorder print helper */
static void 
_bst_print_preorder (bst_node_t *root)
{

}

void 
bst_print_preorder (bst_t *bst, printfn print)
{
	_bst_print_preorder(bst->root);
}
