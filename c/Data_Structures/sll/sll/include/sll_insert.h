#ifndef __SLL_INSERT_H_
#define __SLL_INSERT_H_

/** Insert operations */
int sll_insert_at_front (sll_t *sll, void *data);
int sll_insert_at_end (sll_t *sll, void *data);
int sll_insert_at_position (sll_t *sll, void *data, int pos);
int sll_insert_after (sll_t *sll, void *data, void *key, comparefn compare);

/** 
 * Insert node operations
 *   separated so that an user of SLL doesnt need to be aware of nodes,
 *   and merely use an SLL as an abstract list storage.
 *
 * Node operations need a data encapsulated node and often times don't 
 * check for various corner conditions as it is assumed the caller will perform such checks.
 */
int sll_insert_node_at_front (sll_t *sll, sll_node_t *node);
int sll_insert_node_at_end (sll_t *sll, sll_node_t *node);
int sll_insert_node_at_position (sll_t *sll, sll_node_t *node, int pos);
void sll_insert_after_node (sll_t *sll, sll_node_t *node, sll_node_t *new_node);

#endif
