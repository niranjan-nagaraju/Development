#ifndef __SLL_FIND_H_
#define __SLL_FIND_H_

/** Find operations */
sll_node_t *sll_find_containing_node (sll_t *sll, void *key, comparefn compare);
void *sll_find (sll_t *sll, void *key, comparefn compare);
boolean sll_find_node (sll_t *sll, sll_node_t *node);

#endif
