#include <bst.h>
#include <array_queue.h>


/**
 * Print routines for the binary search tree
 */

/** preorder print helper */
static void 
_bst_print_preorder (bst_node_t *root, printfn print)
{
	if (! root)
		return;

	/** Rlr */
	print(root->data);

	_bst_print_preorder (root->left, print);
	_bst_print_preorder (root->right, print);
}


/** Inorder print helper */
static void 
_bst_print_inorder (bst_node_t *root, printfn print)
{
	if (! root)
		return;

	/** lRr */
	_bst_print_inorder (root->left, print);
	print(root->data);
	_bst_print_inorder (root->right, print);
}


/** postorder print helper */
static void 
_bst_print_postorder (bst_node_t *root, printfn print)
{
	if (! root)
		return;

	/** lrR */
	_bst_print_postorder (root->left, print);
	_bst_print_postorder (root->right, print);
	print(root->data);
}


/** Level order (Breadth first) print helper */
static void
_bst_print_level_order (bst_node_t *root, printfn print, array_queue_t *queue)
{
	bst_node_t *tmp;

	if (! root)
		return;

	print(root->data);

	if (root->left)
		arrayQ_enqueue(queue, (void *)root->left);

	if (root->right)
		arrayQ_enqueue(queue, (void *)root->right);

	tmp = arrayQ_dequeue(queue);

	_bst_print_level_order(tmp, print, queue);
}


/** Main preorder routine */
void 
bst_print_preorder (bst_t *bst, printfn print)
{
	if (!bst)
		return;

	_bst_print_preorder(bst->root, print);
	printNL();
}


/** Main Inorder routine */
void 
bst_print_inorder (bst_t *bst, printfn print)
{
	if (!bst)
		return;

	_bst_print_inorder(bst->root, print);
	printNL();
}


/** Main Postorder routine */
void 
bst_print_postorder (bst_t *bst, printfn print)
{
	if (!bst)
		return;

	_bst_print_postorder(bst->root, print);
	printNL();
}


/** Main Level order (Breadth first) print routine */
void
bst_print_level_order (bst_t *bst, printfn print)
{
	array_queue_t queue;
	if (!bst)
		return;

	arrayQ_init(&queue, 100);

	_bst_print_level_order(bst->root, print, &queue);
	printNL();
}




