#include <sll.h>

/** Initialize SLL book-keeping and function pointers */
void 
init_sll(struct sll_s *this)
{
	this->head = this->tail = NULL;
	this->_size = 0;

	/** Initialize pthread Mutex if threadsafety is enabled */
	SLL_LOCK_INIT(this);

	/** Assign function pointers */
	this->destroy = destroy_sll;

	this->isThreadSafe = isThreadSafe_sll;

	this->next = next_sll;
	this->prev = prev_sll;
	this->value = value_sll;

	this->fromArray = fromArray_sll;
	this->toArray = toArray_sll;

	this->findPos = findPos_sll;
	this->findNode = findNode_sll;
	this->length = length_sll;

	this->objectAt = objectAt_sll;
	this->nodeAt = nodeAt_sll;
	this->objectAtRev = objectAtRev_sll;
	this->nodeAtRev = nodeAtRev_sll;
	this->objectAtFront = objectAtFront_sll;
	this->nodeAtFront = nodeAtFront_sll;
	this->objectAtEnd = objectAtEnd_sll;
	this->nodeAtEnd = nodeAtEnd_sll;

	this->insertAt = insertAt_sll;
	this->insertNodeAt = insertNodeAt_sll;
	this->insertAtRev = insertAtRev_sll;
	this->insertNodeAtRev = insertNodeAtRev_sll;
	this->insertAtFront = insertAtFront_sll;
	this->insertNodeAtFront = insertNodeAtFront_sll;
	this->insertAtEnd = insertAtEnd_sll;
	this->insertNodeAtEnd = insertNodeAtEnd_sll;
	this->insertAfter = insertAfter_sll;
	this->insertNodeAfter = insertNodeAfter_sll;
	this->insertBefore = insertBefore_sll;
	this->insertNodeBefore = insertNodeBefore_sll;

	this->removeObject = removeObject_sll;
	this->removeNode = removeNode_sll;
	this->removeAt = removeAt_sll;
	this->removeNodeAt = removeNodeAt_sll;
	this->removeAtRev = removeAtRev_sll;
	this->removeNodeAtRev = removeNodeAtRev_sll;
	this->removeAfter = removeAfter_sll;
	this->removeNodeAfter = removeNodeAfter_sll;
	this->removeBefore = removeBefore_sll;
	this->removeNodeBefore = removeNodeBefore_sll;
	this->removeFirst = removeFirst_sll;
	this->removeFirstNode = removeFirstNode_sll;
	this->removeLast = removeLast_sll;
	this->removeLastNode = removeLastNode_sll;

	this->print = print_sll;
	this->printR = printR_sll;
	this->printRev = printRev_sll;
	this->printRevR = printRevR_sll;

	this->sort = sort_sll;
	this->place = place_sll;
	this->placeNode = placeNode_sll;
	this->isSorted = isSorted_sll;

	this->reverse = reverse_sll;
	this->reverseR = reverseR_sll;
	this->reverseN = reverseN_sll;
	this->reverseNR = reverseNR_sll;
	this->reverseNRev = reverseNRev_sll;
	this->reverseNRevR = reverseNRevR_sll;

	this->shiftLeft = shiftLeft_sll;
	this->shiftLeftR = shiftLeftR_sll;
	this->shiftRight = shiftRight_sll;
	this->shiftRightR = shiftRightR_sll;
	this->rotateLeft = rotateLeft_sll;
	this->rotateLeftR = rotateLeftR_sll;
	this->rotateRight = rotateRight_sll;
	this->rotateRightR = rotateRightR_sll;

	this->set_union = set_union_sll;
	this->set_intersection = set_intersection_sll;
	this->set_plus = set_plus_sll;
	this->set_minus = set_minus_sll;

	this->hasLoop = hasLoop_sll;
	this->hasLoopR = hasLoopR_sll;
	this->unLoop = unLoop_sll;

	this->hasJoin = hasJoin_sll;
	this->nodeOfJoin = nodeOfJoin_sll;
	this->commonNodes = commonNodes_sll;
}

/** Completely destroy the SLL. If the objects were manually managed, also pass an appropriate routine to free the object storage */
void 
destroy_sll(struct sll_s *this, void (*deallocate)(void *object))
{
	sll_node_t *trav, *prev;

	SLL_LOCK(this);

	trav = this->head;

	while(trav) {
		prev = trav;
		trav = trav->next;

		if (deallocate)
			deallocate(prev->object);

		free(prev);
	}

	this->_size = 0;
	this->head = this->tail = NULL;

	SLL_UNLOCK(this);
}


/** if _MULTI_THREADED_ is defined, the SLL is threadsafe */
int 
isThreadSafe_sll (struct sll_s *this)
{
#ifdef _MULTI_THREADED_
	return 1;
#else
	return 0;
#endif
}

/** Retrieve pointer of node next to 'curr' */
sll_node_t *
next_sll(struct sll_s *this, sll_node_t *curr)
{
	sll_node_t *next;

	if(!this || !curr)
		return NULL;

	SLL_LOCK(this);
	next = curr->next;
	SLL_UNLOCK(this);

	return next;
}

/** 
 * Helper function to retrieve the node previous to the specified node 
 * Caller is assumed to have acquired a lock
 */
sll_node_t *
_prev_sll(sll_node_t *trav, sll_node_t *node)
{
	while (trav && trav->next != node)
		trav = trav->next;

	return trav;
}

/** Retrieve pointer of node previous to 'curr' */
sll_node_t *
prev_sll(struct sll_s *this, sll_node_t *curr)
{
	sll_node_t *node;

	if (!this)
		return NULL;

	SLL_LOCK(this);
	node = _prev_sll(this->head, curr);
	SLL_UNLOCK(this);

	return node;
}

/** Retrieve the value encapsulated in the SLL node */
void *
value_sll(sll_node_t *node)
{
	if(!node)
		return NULL;

	return node->object;
}

/** Construct an SLL from an array */
struct sll_s 
fromArray_sll (void **objects, int n)
{
	struct sll_s sll;

	init_sll(&sll);

	if (!objects)
		return sll;

	while (n-- > 0)
		sll.insertAtFront(&sll, objects[n]);

	return sll;
}

/** Retrieve an array of 'objects' stored in the SLL */
void **
toArray_sll(struct sll_s *this, void **objects)
{
	sll_node_t *trav;
	int i;

	if (!this) {
		return NULL;
	}

	SLL_LOCK(this);
	trav = this->head;

	if (! trav) {
		SLL_UNLOCK(this);
		return NULL;
	}

	if (! objects) {
		*objects = calloc(this->_size, sizeof(void *));

		if (! *objects) {
			SLL_UNLOCK(this);
			return NULL;
		}
	}

	trav = this->head;
	for (i=0; i<this->_size; i++, trav=trav->next) {
		objects[i] = trav->object;
	}
	SLL_UNLOCK(this);

	return objects;
}

/** Retrieve the position in the SLL if a matching 'object' is found */
int 
findPos_sll(struct sll_s *this, void *object, int (*isEqual)(void *obj1, void *obj2))
{
	sll_node_t *trav;
	int pos = 0;

	if (!this)
		return -1;

	/** if no equality function is specified, compare pointers */
	if (!isEqual)
		isEqual = comparePtrs;

	SLL_LOCK(this);
	trav = this->head;

	if (!trav) {
		SLL_UNLOCK(this);
		return -1;
	}

	while (trav && !isEqual(trav->object, object)) {
		trav = trav->next;
		pos ++;
	}
	SLL_UNLOCK(this);

	/** Reached End of SLL, Couldn't find 'object' */
	if (! trav)
		return -1;
	
	return pos;	
}

/** Retrieve the node in the SLL if a matching 'object' is found */
sll_node_t *
findNode_sll(struct sll_s *this, void *object, int (*isEqual)(void *obj1, void *obj2))
{
	sll_node_t *trav;

	if (!this)
		return NULL;

	/** if no equality function is specified, compare pointers */
	if (!isEqual)
		isEqual = comparePtrs;

	SLL_LOCK(this);
	trav = this->head;

	if (!trav) {
		SLL_UNLOCK(this);
		return NULL;
	}

	while (trav && !isEqual(trav->object, object)) {
		trav = trav->next;
	}
	SLL_UNLOCK(this);

	return trav;
}

/** Retrieve the number of nodes in the SLL */
int 
length_sll(struct sll_s *this)
{
	int len;

	if (!this)
		return 0;

	SLL_LOCK(this);
	len = this->_size;
	SLL_UNLOCK(this);

	return len;
}

/** Retrieve object at position */
void *
objectAt_sll(struct sll_s *this, int pos)
{
	sll_node_t *node;

	/** Locate node at specified position and return its content */
	node = nodeAt_sll(this, pos);
	if (node)	
		return node->object;

	return NULL;
}

/** Retrieve node at position */
sll_node_t *
nodeAt_sll(struct sll_s *this, int pos)
{
	sll_node_t *trav;

	if (!this)
		return NULL;

	SLL_LOCK(this);
	trav = this->head;

	/** SLL is empty or not long enough */
	if ((!trav) || (this->_size <= pos)) {
		SLL_UNLOCK(this);
		return NULL;
	}

	while (trav && pos-- > 0) {
		trav = trav->next;
	}

	SLL_UNLOCK(this);

	return trav;
}

/** Retrieve object at position from the end */
void *
objectAtRev_sll(struct sll_s *this, int rpos)
{
	sll_node_t *node;

	node = nodeAtRev_sll(this, rpos);
	if (node)
		return node->object;

	return NULL;
}

/** Retrieve node at position from the end */
sll_node_t *
nodeAtRev_sll(struct sll_s *this, int rpos)
{
	int pos, size;

	if (!this)
		return NULL;

	size = length_sll(this);

	/** Calculate position from left => (n-r-1) */
	pos = size - rpos - 1;
	
	return nodeAt_sll(this, pos);
}

/** Retrieve object at the beginning of the SLL */
void *
objectAtFront_sll(struct sll_s *this)
{
	sll_node_t *node;

	node = nodeAtFront_sll(this);
	if (node)
		return node->object;

	return NULL;
}

/** Retrieve node at the beginning of the SLL */
sll_node_t *
nodeAtFront_sll(struct sll_s *this)
{
	sll_node_t *node;

	if (!this)
		return NULL;

	SLL_LOCK(this);
	node = this->head;
	SLL_UNLOCK(this);

	return node;
}

/** Retrieve object at the end of the SLL */
void *
objectAtEnd_sll(struct sll_s *this)
{
	sll_node_t *node;

	node = nodeAtEnd_sll(this);
	if (node)
		return node->object;

	return NULL;
}

/** Retrieve node at the end of the SLL */
sll_node_t *
nodeAtEnd_sll(struct sll_s *this)
{
	sll_node_t *node;

	if (!this)
		return NULL;

	SLL_LOCK(this);
	node = this->tail;
	SLL_UNLOCK(this);

	return node;
}
