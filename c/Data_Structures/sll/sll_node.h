#ifndef __SLL_NODE_H_
#define __SLL_NODE_H_

#include <stdlib.h>

typedef struct sll_node_s {
	void *object;
	struct sll_node_s *next;
} sll_node_t;

sll_node_t *new_sll_node(void *object); /** Create a SLL node encapsulation of the "object" to be inserted into the SLL */
void *free_sll_node(sll_node_t *node); /** Free the encapsulated SLL node and return its content */
void *value_sll_node(sll_node_t *node);	/* Fetch the object encapsulated in the node */

#endif
