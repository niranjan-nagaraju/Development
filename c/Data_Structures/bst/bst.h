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

#ifdef _MULTI_THREADED_
	pthread_mutex_t lock;
#endif

} bst_t;

void init_bst(bst_t *tree);

#endif
