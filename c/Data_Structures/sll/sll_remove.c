#include <sll.h>

/** Delete the specified node off the SLL and return the object; Use the previous node reference to keep the chain coherent */
static void * _removeObjectAtNode(struct sll_s *this, sll_node_t *prev, sll_node_t *node);

/** Delete the specified node off the SLL; Use the previous node reference to keep the chain coherent */
static void _removeNode(struct sll_s *this, sll_node_t *prev, sll_node_t *node);

/** Delete the specified node off the SLL; Use the previous node reference to keep the chain coherent
 *  No checks performed -- UNSAFE to be called directly
 *  Needs Lock context
 *  FAIR WARNING: DO NOT call on an empty list.
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
 *  FAIR WARNING: DO NOT call on an empty list.
 */
static void *
_removeObjectAtNode(struct sll_s *this, sll_node_t *prev, sll_node_t *node)
{
	_removeNode(this, prev, node);
	return free_sll_node(node);
}

/** Remove an SLL node matching 'object' in the SLL and return its content */
void *(*removeMatchingObj)(struct sll_s *this, void *object, comparator compare)	

/** Remove an SLL node matching 'object' in the SLL */
void *
removeObject_sll(struct sll_s *this, void *object, comparefn compare)
{
	sll_node_t *trav, *prev;
	void *object_to_return;

	if (!this)
		return NULL;

	SLL_LOCK(this);
	trav = this->head;

	prev = NULL;
	while (trav && (compare(trav->object, object) == 0)) {
		prev = trav;
		trav = trav->next;
	}

	/** Couldn't find a matching node containing "object" */
	if (! trav) {
		SLL_UNLOCK(this);
		return NULL;
	}

	/** Remove the node from the SLL */
	object_to_return = _removeObjectAtNode(this, prev, trav);

	SLL_UNLOCK(this);

	return object_to_return;
}


/** Remove the object at specified 'node' from the SLL */
void *
removeObjectAtNode_sll (struct sll_s *this, sll_node_t *node)
{
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

	if (!this || !node)
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

	if (!this)
		return NULL;

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

	/** NOTE: Consider -pos to remove from reverse?? */
	if (!this || pos <= 0)
		return NULL;

	SLL_LOCK(this);

	trav = this->head;
	while (trav && --pos) {
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
	void *object;
	sll_node_t *node;

	if (!this)
		return NULL;

	/** No locks required here, because removeNodeAtRev_sll yanks the node off the SLL and we are just dealing with an orphaned node */
	node = removeNodeAtRev_sll(this, pos);

	if (! node)
		return NULL;

	object = node->object;

	free_sll_node(node);
	
	return object;
}


/** 
 * Remove an SLL node at position from the end and return it
 * NOTE: ALTERNATE METHOD:
 *   Use trav and travN, make travN trail trav by 'pos'.
 *   When trav falls off the SLL, travN points to 'pos' node from end.
 */   
sll_node_t *
removeNodeAtRev_sll(struct sll_s *this, int pos)
{
	int size, lpos;

	if (!this)
		return NULL;

	/** Calculate position from left */
	lpos = size - pos + 1;

	return removeNodeAt_sll(this, lpos);
}


/** Remove an object following the specified 'node' and return its content */
void *
removeAfter_sll(struct sll_s *this, sll_node_t *node)
{
	void *object;

	if (!this || !node)
		return NULL;

	SLL_LOCK(this);
	object = node->object;
	_removeNode(this, node, node->next);

	if (! node)
		return NULL;

	object = node->object;


	SLL_UNLOCK(this);

	return object;
}

sll_node_t *removeNodeAfter_sll(struct sll_s *this, sll_node_t *node)
{
	if (!this || !node)
		return NULL;

	SLL_LOCK(this);

	if (!node->next) {
		SLL_UNLOCK(this);
		return NULL;
	}
	node_removed = node->next;
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
