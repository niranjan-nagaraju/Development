#include <sll_internal.h>

/** Return a SLL node containing key */
sll_node_t *
sll_find_containing_node (sll_t *sll, void *key, comparefn compare)
{
	sll_node_t *tmp;

	/** SLL doesn't exist or is empty */
	if (!sll || !sll->head)
		return NULL;

	tmp = sll->head;

	while (tmp != NULL && compare(tmp->data, key) != 0) {
		tmp = tmp->next;
	}


	return tmp;
}


/** Return node data if node containing key is found in the SLL */
void *
sll_find (sll_t *sll, void *key, comparefn compare)
{
	sll_node_t *tmp;

	tmp = sll_find_containing_node (sll, key, compare);

	if (!tmp)
		return NULL;

	return tmp->data;
}


/** Return True if node exists in SLL, False otherwise */
boolean
sll_find_node (sll_t *sll, sll_node_t *node)
{
	sll_node_t *tmp;

	/** SLL doesn't exist or is empty */
	if (!sll || !sll->head)
		return false;

	tmp = sll->head;
	
	while (tmp && tmp != node) {
		tmp = tmp->next;
	}

	return ((tmp == NULL)? false : true);
}
