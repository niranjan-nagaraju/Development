#include <sll.h>

/** Delete the specified node off the SLL and return the object; Use the previous node reference to keep the chain coherent */
static void *_removeObjectAtNode(struct sll_s *this, sll_node_t *prev, sll_node_t *node);

/** Delete the specified node off the SLL; Use the previous node reference to keep the chain coherent */
static void _removeNode(struct sll_s *this, sll_node_t *prev, sll_node_t *node);

/** Delete the specified node off the SLL; Use the previous node reference to keep the chain coherent
 *  No checks performed -- UNSAFE to be called directly
 *  Needs Lock context
 */
static void 
_removeNode(struct sll_s *this, sll_node_t *prev, sll_node_t *node)
{
	/** In here, A node is always removed. */
	(this->_size)--;

	/** If the node to be deleted is the first one in the SLL, update head */
	if (node == this->head)
		this->head = node->next;

	/** If the node to be deleted is the last one in the SLL, update tail */
	if (! node->next)
		this->tail = node;

	prev->next = node->next; /** Yank it out of the chain first */
}


/** Delete the specified node off the SLL and return the object; Use the previous node reference to keep the chain coherent 
 *  No checks performed -- UNSAFE to be called directly
 *  Needs Lock context
 */
static void *
_removeObjectAtNode(struct sll_s *this, sll_node_t *prev, sll_node_t *node)
{
	void *object;
	
	object = node->object; /** Get the encapsulated object */
	_removeNode(this, prev, node);
	free_sll_node(node);

	return object;
}


/** Remove an SLL node matching 'object' in the SLL */
void 
removeObject_sll(struct sll_s *this, void *object)
{
	sll_node_t *trav, *prev;

	SLL_LOCK(this);
	trav = this->head;

	prev = NULL;
	while (trav && trav->object != object) {
		prev = trav;
		trav = trav->next;
	}

	/** Couldn't find a matching node containing "object" */
	if (! trav) {
		SLL_UNLOCK(this);
		return;
	}

	/** Remove the node from the SLL */
	_removeObjectAtNode(this, prev, trav);

	SLL_UNLOCK(this);
}


/** Remove the specified 'node' from the SLL 
 *  Swap node contents with the one that follows the specified node in the SLL
 *  and delete the next node as is usually done.
 */
void *
removeNode_sll(struct sll_s *this, sll_node_t *node)
{
	sll_node_t *follow;
	void *object;

	if (!node)
		return NULL;

	SLL_LOCK(this);

	/** We don't actually verify if the "node" actually belongs to the SLL or not
	 *  But if the specified SLL is empty, we can be sure it doesn't
	 *  So we don't attempt to delete even if "node" is actually a part of some chain and can indeed be deleted.
	 */
	if (! this->head ) {
		SLL_UNLOCK(this);
		return NULL;
	}

	/** Retrieve the node's value and store it to be returned at the end */
	object = node->object;

	follow = node->next;
	node->object = follow->object;	/** 'node' and its following node both contain the same object now, so it's immaterial which one we delete,
									  * We delete the following node 'cause it's straightforward */
	_removeObjectAtNode(this, node, follow);	/** Remove the node following the one specified */

	SLL_UNLOCK(this);

	return object;
}


/** Remove an object at position and return its content */
void *
removeAt_sll(struct sll_s *this, int pos)
{
	void *object;
	sll_node_t *node;

	/** No locks required here, because removeNodeAt_sll yanks the node off the SLL and we are just dealing with an orphaned node */
	node = removeNodeAt_sll(this, pos);

	if (! node)
		return NULL;

	object = node->object;

	free_sll_node(node);
	
	return object;
}


/** Remove an SLL node at position and return it */
sll_node_t *
removeNodeAt_sll(struct sll_s *this, int pos)
{
	sll_node_t *trav;

	SLL_LOCK(this);

	trav = this->head;
	while (trav && pos--) {
		trav = trav->next;
	}

	if (! trav) {
		SLL_UNLOCK(this);
		return NULL;
	}

	 _removeNode(this, trav, trav->next);

	SLL_UNLOCK(this);

	return trav;
}


/** Remove an object at position from the end and return its content */
void *
removeAtRev_sll(struct sll_s *this, int pos)
{
	return NULL;
}

sll_node_t *removeNodeAtRev_sll(struct sll_s *this, int pos)
{
	return NULL;
}

void *removeAfter_sll(struct sll_s *this, sll_node_t *node)
{
	return NULL;
}

sll_node_t *removeNodeAfter_sll(struct sll_s *this, sll_node_t *node)
{
	return NULL;
}

void *removeBefore_sll(struct sll_s *this, sll_node_t *node)
{
	return NULL;
}

sll_node_t *removeNodeBefore_sll(struct sll_s *this, sll_node_t *node)
{
	return NULL;
}

void *removeFirst_sll(struct sll_s *this)
{
	return NULL;
}

sll_node_t *removeFirstNode_sll(struct sll_s *this)
{
	return NULL;
}


void *removeLast_sll(struct sll_s *this)
{
	return NULL;
}

sll_node_t *removeLastNode_sll(struct sll_s *this)
{
	return ;
}
