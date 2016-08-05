#include <sll.h>
#include <sll_remove.h>

#define REMOVE_AT_FRONT 1
#define REMOVE_AT_END	2
#define REMOVE_AT_POS	3

/** Helper function to remove from the SLL */
static void *
_sll_remove (sll_t *sll, int cmd, int pos)
{
	sll_node_t *node;
	void *data;

	/** sll doesn't exist or is empty */
	if (!sll || !sll->head)
		return NULL;

	switch (cmd) {
		case REMOVE_AT_FRONT:
			node = sll_remove_node_at_front(sll);
			break;
		case REMOVE_AT_END:
			node = sll_remove_node_at_end(sll);
			break;
		case REMOVE_AT_POS:
			node = sll_remove_node_at_position(sll, pos);
			break;
	}

	if (node) {
		data = node->data;
		free(node);

		return data;
	}

	return NULL;
}


/** Remove a node at the beginning of the SLL */
sll_node_t *
sll_remove_node_at_front (sll_t *sll)
{
	sll_node_t *node = sll->head;

	/** slide head to the right */
	sll->head = sll->head->next;
	sll->_size--;

	/** node just removed was the only node in the SLL */
	if (! sll->head)
		sll->tail = NULL;

	return node;
}


/** Remove an element from the front of the SLL */
void *
sll_remove_at_front (sll_t *sll)
{
	return _sll_remove(sll, REMOVE_AT_FRONT, 0);
}


/** Remove a node from the end of the SLL */
sll_node_t *
sll_remove_node_at_end (sll_t *sll)
{
	sll_node_t *node, *trav;

	sll->_size--;

	/** Traverse till penultimate node in the SLL */
	trav = sll->head;
	/** TODO: optimize condition using _size and index */
	while (trav->next && trav->next != sll->tail) {
		trav = trav->next;
	}

	/** backup current tail */
	node = sll->tail;

	trav->next = NULL;
	sll->tail = trav;

	/** 
	 * Removing the node rendered the list empty;
	 * Update head
	 */ 
	if (!sll->_size) {
		sll->head = NULL;
	}

	return node;
}


/** Remove an element from the end of the SLL */
void *
sll_remove_at_end (sll_t *sll)
{
	return _sll_remove(sll, REMOVE_AT_END, 0);
}


/** Remove a node from the specified position */
sll_node_t *
sll_remove_node_at_position (sll_t *sll, int pos)
{
	sll_node_t *node, *trav;
	int i = 0;

	if (pos >= sll->_size)
		return NULL;

	if (pos == 0)
		return sll_remove_node_at_front(sll);

	trav = sll->head;
	while (i < (pos-1)) {
		trav = trav->next;
		i++;
	}

	sll->_size--;

	/** trav now points to the node before the one that needs to be removed */
	node = trav->next;

	/** NOTE: 
	 *  if sll had only one element in it at this point, pos could only have been 0 and that's handled by (pos==0)
	 *  So trav->next->next will not run into a NULL pointer exception
	 */
	trav->next = trav->next->next;

	/** node removed was last in the SLL; update tail */
	if (! trav->next)
		sll->tail = trav;

	return node;
}


/** Remove an element from a specified position */
void *
sll_remove_at_position (sll_t *sll, int pos)
{
	return _sll_remove(sll, REMOVE_AT_POS, pos);
}

