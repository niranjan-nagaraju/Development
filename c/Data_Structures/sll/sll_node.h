#ifndef _SLL_NODE_H

#define _SLL_NODE_H

#include <stdlib.h>

typedef struct sll_node_s {
	void *data;
	struct sll_node_s *next;
} sll_node_t;

sll_node_t *create_sll_node (void *data);
void *delete_sll_node (sll_node_t *node);

#endif //_SLL_NODE_H
