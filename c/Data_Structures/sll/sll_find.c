#include <sll.h>

/** Return a SLL node matching key */
sll_node_t *
sll_find_node (sll_t *sll, void *key, comparefn compare)
{
	sll_node_t *tmp;

	/** SLL doesn't exist or is empty */
	if (!sll || !sll->head)
		return NULL;

	tmp = sll->head;

	while (tmp != NULL && compare(tmp->data, key) != 0)
		tmp = tmp->next;


	return tmp;
}


/** Return node data if node containing key is found in the SLL */
void *
sll_find (sll_t *sll, void *key, comparefn compare)
{
	sll_node_t *tmp;

	tmp = sll_find_node (sll, key, compare);

	if (!tmp)
		return NULL;

	return tmp->data;
}
