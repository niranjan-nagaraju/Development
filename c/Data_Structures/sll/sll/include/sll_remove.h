#ifndef __SLL_REMOVE_H_
#define __SLL_REMOVE_H_

/** Remove operations */
void *sll_remove_at_front (sll_t *sll);
void *sll_remove_at_end (sll_t *sll);
void *sll_remove_at_position (sll_t *sll, int pos);

sll_node_t *sll_remove_node_at_front (sll_t *sll);
sll_node_t *sll_remove_node_at_end (sll_t *sll);
sll_node_t *sll_remove_node_at_position (sll_t *sll, int pos);
#endif

