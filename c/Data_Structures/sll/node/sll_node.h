#ifndef _SLL_NODE_H

#define _SLL_NODE_H

#include <stdlib.h>
#include <errno.h>

typedef struct sll_node_s {
	void *data;
	struct sll_node_s *next;
} sll_node_t;

sll_node_t *sll_node_create (void *data);
void *sll_node_delete (sll_node_t *node);
void sll_node_print (sll_node_t *node, void (*printfn)(void *));

#endif //_SLL_NODE_H
