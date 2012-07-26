#ifndef _BINARY_SEARCH_TREE_H_
#define _BINARY_SEARCH_TREE_H_

#include <bst_node.h>
#include <errno.h>

#include <common.h>

typedef struct bst_s {
	bst_node_t *root;
	int _num_nodes;
} bst_t;

void bst_init(bst_t *tree);
int bst_size (bst_t *tree);
int bst_width (bst_t *tree);
int bst_depth (bst_t *tree);

#endif /* _BINARY_SEARCH_TREE_H_ */
