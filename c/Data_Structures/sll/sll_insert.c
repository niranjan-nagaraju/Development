#include <sll.h>
#include <sll_internal.h>

/** Helper to Insert an SLL node at a specified position in the SLL; No validation done */
static void _insertNodeAt_sll(sll_t *this, sll_node_t *node, int pos);

/** 
 * Helper to insert a node at a specified position
 * No Validation for node or pos 
 */
static void 
_insertNodeAt_sll(sll_t *this, sll_node_t *node, int pos)
{
	sll_node_t *head;
	int i;
	int orig_size;

	SLL_LOCK(this);
	orig_size = (this->_size)++; /** Update size since Insert will(should) never fail */
	head = this->head;

	/** If the SLL is empty, Insert this node at the start and return */
	if (pos == 0 || !head) {
		node->next = head;
		this->head = node;

		if (!head) /** Update tail if the list was initially empty */
			this->tail = node;

		SLL_UNLOCK(this);
		return;
	}
	
	/** position exceeds curr range; add at end */
	if (orig_size < pos) {
		this->tail->next = node;
		this->tail = node;

		SLL_UNLOCK(this);
		return;
	}

	i = 1;
	while (i++ < pos)
		head = head->next;

	node->next = head->next;
	head->next = node;

	SLL_UNLOCK(this);
}


/** Insert an object at a specified position in the SLL */
int 
insertAt_sll(struct sll_s *this, void *object, int pos)
{
	sll_node_t *tmp;

	if (!this || pos < 0)
		return -EINVAL;

	/** Encapsulate object in node to begin with */
	tmp = new_sll_node(object);
	if (!tmp)
		return -ENOMEM;

	_insertNodeAt_sll(this, tmp, pos);

	return 0;
}


/** Insert an SLL node at a specified position */
int 
insertNodeAt_sll(struct sll_s *this, sll_node_t *node, int pos)
{
	if (!this || pos < 0 || !node)
		return -EINVAL;

	_insertNodeAt_sll(this, node, pos);
	return 0;
}


/** Insert an object at a specified position from the end */
int 
insertAtRev_sll(struct sll_s *this, void *object, int pos)
{
	sll_node_t *tmp;

	/** 
	 * NOTE: "this" is checked at insertNodeXXX as well, however, we might leak memory out if we dont check it here
	 * and the nodeXXX function fails
	 */
	if (!this)
		return -EINVAL;
	
	/** Encapsulate object in node to begin with */
	tmp = new_sll_node(object);
	if (!tmp)
		return -ENOMEM;
	
	return insertNodeAtRev_sll(this, tmp, pos);
}


/**
 * Insert an SLL node at a specified position from the end 
 * NOTE: ALTERNATE METHOD:
 *   Use trav and travN, make travN trail trav by 'pos'.
 *   When trav falls off the SLL, travN points to 'pos' node from end.
 */
int 
insertNodeAtRev_sll(struct sll_s *this, sll_node_t *node, int pos)
{
	int size, lpos;

	if (!this || !node || pos < 0)
		return -EINVAL;

	/** Calculate pos from left */
	SLL_LOCK(this);
	size = this->_size;
	SLL_UNLOCK(this);
	
	lpos = size - pos;	
	if (lpos < 0)
		return -EINVAL;

	_insertNodeAt_sll(this, node, lpos);
	return 0;
}


/**
 * Insert a node at the beginning of SLL
 * UNSAFE/FAST version
 * NO checks performed.
 * Assumes locks are already acquired
 *
 * Lockless version to be used for "fromArray"
 */
void
_insertNodeAtFront_sll(struct sll_s *this, sll_node_t *node)
{
	++(this->_size);
	node->next = this->head;

	/** SLL empty; Update tail */
	if (!this->head)
		this->tail = node;

	this->head = node;
}


/** Insert at the beginning of the SLL */
int 
insertAtFront_sll(struct sll_s *this, void *object)
{
	sll_node_t *tmp;

	/** 
	 * NOTE: "this" is checked at insertNodeXXX as well, however, we might leak memory out if we dont check it here
	 * and the nodeXXX function fails
	 */
	if (!this)
		return -EINVAL;

	tmp = new_sll_node(object);
	if (!tmp)
		return -ENOMEM;

	SLL_LOCK(this);
	_insertNodeAtFront_sll(this, tmp);
	SLL_UNLOCK(this);

	return 0;
}


/** Insert an SLL node at the beginning */
int 
insertNodeAtFront_sll(struct sll_s *this, sll_node_t *node)
{
	if (!this ||!node)
		return -EINVAL;

	SLL_LOCK(this);
	_insertNodeAtFront_sll(this, node);	
	SLL_UNLOCK(this);

	return 0;
}


/** Insert at the end of the SLL */
int 
insertAtEnd_sll(struct sll_s *this, void *object)
{
	sll_node_t *tmp;

	/** 
	 * NOTE: "this" is checked at insertNodeXXX as well, however, we might leak memory out if we dont check it here
	 * and the nodeXXX function fails
	 */
	if (!this)
		return -EINVAL;

	tmp = new_sll_node(object);
	if (!tmp)
		return -ENOMEM;

	return insertNodeAtEnd_sll(this, tmp);
}


/** Insert an SLL node at the end */
int 
insertNodeAtEnd_sll(struct sll_s *this, sll_node_t *node)
{
	if (!this || !node)
		return -EINVAL;

	SLL_LOCK(this);

	++(this->_size);

	/** SLL Empty */
	if (!this->tail) {
		this->tail = this->head = node;
		SLL_UNLOCK(this);

		return 0;
	}

	(this->tail)->next = node;
	this->tail = node;

	SLL_UNLOCK(this);

	return 0;
}

/** Insert an object after the specified 'node' */
int 
insertAfter_sll(struct sll_s *this, sll_node_t *node, void *object)
{
	sll_node_t *tmp;
	
	/** 
	 * NOTE: "this" is checked at insertNodeXXX as well, however, we might leak memory out if we dont check it here
	 * and the nodeXXX function fails
	 */
	if (!this)
		return -EINVAL;

	tmp = new_sll_node(object);
	if (!tmp)
		return -ENOMEM;

	return insertNodeAfter_sll(this, node, tmp);
}


/** Insert an SLL node after the specified 'node' */
int 
insertNodeAfter_sll(struct sll_s *this, sll_node_t *node, sll_node_t *node_to_insert)
{
	if (!this || !node || !node_to_insert)
		return -EINVAL;

	++(this->_size);
	node_to_insert->next = node->next;
	node->next = node_to_insert;

	return 0;
}


/** Insert an object before the specified 'node' */
int 
insertBefore_sll(struct sll_s *this, sll_node_t *node, void *object)
{
	sll_node_t *tmp;

	/** 
	 * NOTE: "this" is checked at insertNodeXXX as well, however, we might leak memory out if we dont check it here
	 * and the nodeXXX function fails
	 */
	if (!this)
		return -EINVAL;

	tmp = new_sll_node(object);
	if (!tmp)
		return -ENOMEM;

	return insertNodeBefore_sll(this, node, tmp);
}


/** Insert an SLL node before the specified 'node'
 *  Insert "after" node, and swap contents (saves traversing time)
 */
int
insertNodeBefore_sll(struct sll_s *this, sll_node_t *node, sll_node_t *node_to_insert)
{
	void *node_content;

	if (!this || !node || !node_to_insert)
		return -EINVAL;

	SLL_LOCK(this);

	++(this->_size);

	/** swap contents */
	node_content = node->object;
	node->object = node_to_insert->object;
	node_to_insert->object = node_content;

	/** insert new node "next" to specified node */
	node_to_insert->next = node->next;
	node->next = node_to_insert;

	SLL_UNLOCK(this);

	return 0;
}

