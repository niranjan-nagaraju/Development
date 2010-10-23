#ifndef _BINARY_SEARCH_TREE_H_
#define _BINARY_SEARCH_TREE_H_

#include <stdlib.h>
#include <pthread.h>
#include <errno.h>

#include <common.h>

#ifdef _MULTI_THREADED_
	#define	BST_LOCK_INIT(bst)	pthread_mutex_init(&(bst->lock), NULL)
	#define BST_LOCK(bst)		pthread_mutex_lock(&(bst->lock))
	#define BST_UNLOCK(bst)		pthread_mutex_unlock(&(bst->lock))
#else
	#define	BST_LOCK_INIT(bst) 
	#define BST_LOCK(bst)
	#define BST_UNLOCK(bst)
#endif

typedef struct node_s {
	void *object;
	struct node_s *left;
	struct node_s *right;
} node_t;

typedef struct bst_s {
	node_t *root;

	int _num_nodes;
	int _width;
	int _depth;

#ifdef _MULTI_THREADED_
	pthread_mutex_t lock;
#endif

	node_t *(*newNode) (void *object);
	void *(*value) (node_t *node);

	void (*insert) (struct bst_s *tree, void *object, int (*compare)(void *object1, void *object2));
	void (*insert_node) (struct bst_s *tree, node_t *node, int (*compare)(void *object1, void *object2));

	void (*remove) (struct bst_s *tree, void *object, int (*compare)(void *object1, void *object2));
	void (*remove_node) (struct bst_s *tree, node_t *node, int (*compare)(void *object1, void *object2));

	int (*depth) (struct bst_s *tree);
	int (*width) (struct bst_s *tree);
	int (*numNodes) (struct bst_s *tree);

	int (*calculate_depthR) (struct bst_s *tree);
	int (*calculate_widthR) (struct bst_s *tree);
	int (*calculate_numNodes) (struct bst_s *tree);

	int (*minimum) (struct bst_s *tree);
	int (*maximum) (struct bst_s *tree);
	int (*nthLargest) (struct bst_s *tree, int n);
	int (*nthSmallest) (struct bst_s *tree, int n);

	int (*isStructurallyMirrored) (struct bst_s *tree);	/** Are right and Left Subtrees structurally a mirror images of each other */
	int (*isStructurallyMirrored2) (struct bst_s *tree1, struct bst_tree *tree2 ); /** Are the trees structurally a mirror images of each other */

	void *(*toContainerPreOrder) (struct bst_s *tree, void (*insertfn)(void *args));
	void *(*toContainerInOrder) (struct bst_s *tree, void (*insertfn)(void *args));
	void *(*toContainerPostOrder) (struct bst_s *tree, void (*insertfn)(void *args));
	void *(*toContainerDepthFirst) (struct bst_s *tree, void (*insertfn)(void *args));
	void *(*toContainerBreadthFirst) (struct bst_s *tree, void (*insertfn)(void *args));

	void (*printPreOrder) (struct bst_s *tree, (void (*printfn)(void *object)));
	void (*printInOrder) (struct bst_s *tree, (void (*printfn)(void *object)));
	void (*printPostOrder) (struct bst_s *tree, (void (*printfn)(void *object)));
	void (*printDepthFirst) (struct bst_s *tree, (void (*printfn)(void *object)));
	void (*printBreadthFirst) (struct bst_s *tree, (void (*printfn)(void *object)));
	void (*printLevelOrder) (struct bst_s *tree, (void (*printfn)(void *object)));
	void (*printLevelOrderFormatted) (struct bst_s *tree, (void (*printfn)(void *object)));

} bst_t;

void init_bst(bst_t *tree);

#endif
