#include <sll.h>

#define INSERT_AT_FRONT	1
#define INSERT_AT_END	2
#define INSERT_AT_POS	3

/** Helper function to insert into the SLL */
static int
_sll_insert (sll_t *sll, int cmd, void *data, int pos)
{
	sll_node_t *node = NULL;
	
	if (!sll)
		return -1;

	node = sll_node_create (data);

	if (!node)
		return -ENOMEM;

	switch (cmd) {
		case INSERT_AT_FRONT:
			return sll_insert_node_at_front(sll, node);
		case INSERT_AT_END:
			return sll_insert_node_at_end(sll, node);
		case INSERT_AT_POS:
			return sll_insert_node_at_position(sll, node, pos);
	}
	
	return 0;
}


/** Insert a node at the beginning of the SLL */
int 
sll_insert_node_at_front (sll_t *sll, sll_node_t *node)
{
	/** Insert never fails :) */
	sll->_size++;

	node->next = sll->head;
	sll->head = node;

	/** SLL is empty */
	if ( sll->tail == NULL )
		sll->tail = node;

	return 0;
}

/** Insert at the beginning of the SLL */
int 
sll_insert_at_front (sll_t *sll, void *data)
{
	return _sll_insert(sll, INSERT_AT_FRONT, data, 0);
}


/** Insert a node at the end of the SLL */
int
sll_insert_node_at_end (sll_t *sll, sll_node_t *node)
{
	/** Insert never fails :) */
	sll->_size++;

	/** SLL is empty */
	if (! sll->tail) {
		sll->tail = sll->head = node;
		return 0;
	}

	sll->tail->next = node;
	sll->tail = node;

	return 0;
}


/** Insert at the end of the SLL */
int 
sll_insert_at_end (sll_t *sll, void *data)
{
	return _sll_insert(sll, INSERT_AT_END, data, 0);
}


/** Insert SLL at a specified position */
int 
sll_insert_node_at_position (sll_t *sll, sll_node_t *node, int pos)
{
	int i;
	sll_node_t *trav, *prev;

	if ( pos == 0 )
		return sll_insert_node_at_front(sll, node);
	
	/** SLL is empty */
	if ( (! sll->head ) || ( pos > sll->_size ) )
		return -1;

	i = 0;
	trav = sll->head;
	while (i < pos-1) {
		trav = trav->next;
		i++;
	}

	/** trav now points to (pos-1), so inserting node after that would make it the 'pos'th node starting from 0 */
	node->next = trav->next;
	trav->next = node;

	sll->_size++;

	/** Newly inserted node is at the end; Update tail */
	if (! node->next ) {
		sll->tail = node;
	}

	return 0;
}


/** Insert SLL at a specified position */
int 
sll_insert_at_position (sll_t *sll, void *data, int pos)
{
	return _sll_insert(sll, INSERT_AT_POS, data, pos);
}
