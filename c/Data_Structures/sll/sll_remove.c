#include <sll.h>

/** Delete the specified node off the SLL; Use the previous node reference to keep the chain coherent */
static void _removeNodeAfter(sll_node_t *node);	

/** Delete the specified node off the SLL; Use the previous node reference to keep the chain coherent
 *  No checks performed -- UNSAFE to be called directly
 */
static void*
_removeNodeAfter(struct sll_s *this, sll_node_t *prev, sll_node_t *node)
{
	void *object;

	object = node->object;	/** Get the encapsulated object */
	prev->next = node->next;	/** Yank it out of the chain first */
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

	prev = trav;
	while (trav && trav->object != object) {
		prev = trav;
		trav = trav->next;
	}

	/** Couldn't find a matching node containing "object" */
	if (! trav) {
		SLL_UNLOCK(this);
		return NULL;
	}

	/** Remove the node from the SLL */
	_removeNodeAfter(prev, trav);

	SLL_UNLOCK(this);

	return NULL;
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

	SLL_UNLOCK(this);

	/** We don't actually verify if the "node" actually belongs to the SLL or not
	 *  But if the specified SLL is empty, we can be sure it doesn't
	 *  So we don't attempt to delete even if "node" is actually a part of some chain and can indeed be deleted.
	 */
	if (! this->head ) {
		SLL_UNLOCK(this);
		return NULL;
	}

	follow = node->next;
	if (!follow) {

	}

	return node;
}

void *removeAt_sll(struct sll_s *this, int pos)
{
	return NULL;
}

sll_node_t *removeNodeAt_sll(struct sll_s *this, int pos)
{
	return NULL;
}

void *removeAtRev_sll(struct sll_s *this, int pos)
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
	return NULL;
}
