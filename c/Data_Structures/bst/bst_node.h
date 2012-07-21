#ifndef _BST_NODE_H

#define _BST_NODE_H

#include <stdlib.h>
#include <errno.h>

typedef struct bst_node_s {
	void *data;
	struct bst_node_s *left;
	struct bst_node_s *right;
} bst_node_t;

bst_node_t *bst_node_create (void *data);
void *bst_node_delete (bst_node_t *node);
void bst_node_print (bst_node_t *node, void (*printfn)(void *));

#endif //_bst_NODE_H
