#ifndef _BST_NODE_H

#define _BST_NODE_H

#include <errno.h>
#include <common.h>
#include <stdlib.h>

typedef struct bst_node_s {
	void *data;
	struct bst_node_s *left;
	struct bst_node_s *right;
} bst_node_t;

bst_node_t *bst_node_create (void *data);
void *bst_node_delete (bst_node_t *node);
int bst_node_has_left (bst_node_t *node);
int bst_node_has_right (bst_node_t *node);
int bst_node_is_leaf (bst_node_t *node);
void bst_node_print (bst_node_t *node, void (*printfn)(void *));

/** Insert operations */
int bst_node_insert_to_left (bst_node_t *node, void *data);
int bst_node_insert_node_to_left (bst_node_t *parent, bst_node_t *node);
int bst_node_insert_to_right (bst_node_t *node, void *data);
int bst_node_insert_node_to_right (bst_node_t *parent, bst_node_t *node);


#endif //_BST_NODE_H
