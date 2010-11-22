#include <sll.h>

/** Helper to Insert an SLL node at a specified position in the SLL; No validation done */
static int _insertNodeAt_sll(sll_t *this, sll_node *node, int pos);

/** No Validation for node or pos */
static int 
_insertNodeAt_sll(sll_t *this, sll_node *node, int pos)
{
	sll_node *head;
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

		return 0;
	}
	
	/** position exceeds curr range; add at end */
	if (orig_size < pos) {
		this->tail->next = node;
		this->tail = node;

		return 0;
	}

	i = 1;
	while (i++ < pos)
		head = head->next;

	node->next = head->next;
	head->next = node;

	return 0;
}

/** Insert an object at a specified position in the SLL */
int 
insertAt_sll(struct sll_s *this, void *object, int pos)
{
	sll_node *tmp;

	if (!this || pos < 0)
		return -EINVAL;

	/** Encapsulate object in node to begin with */
	tmp = this->newNode(object);
	if (!tmp)
		return -ENOMEM;

	return _insertNodeAt_sll(this, tmp, pos);
}

/** Insert an SLL node at a specified position */
int 
insertNodeAt_sll(struct sll_s *this, sll_node *node, int pos)
{
	if (!this || pos < 0 || !node)
		return -EINVAL;

	return _insertNodeAt_sll(this, node, pos);
}

/** Insert an object at a specified position from the end */
int 
insertAtRev_sll(struct sll_s *this, void *object, int pos)
{
	sll_node *tmp;

	/** More validations in insertNode... */
	if (!this)
		return -EINVAL;
	
	/** Encapsulate object in node to begin with */
	tmp = this->newNode(object);
	if (!tmp)
		return -ENOMEM;
	
	return insertNodeAtRev_sll(this, tmp, pos);
}

/** Insert an SLL node at a specified position from the end */
int 
insertNodeAtRev_sll(struct sll_s *this, sll_node *node, int pos)
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

	return _insertNodeAt_sll(this, node, lpos);
}

int 
insertAtFront_sll(struct sll_s *this, void *object)
{
	sll_node *tmp;

	tmp = this->newNode(object);
	if (!tmp)
		return -ENOMEM;

	return insertNodeAtFront_sll(this, tmp);
}

int 
insertNodeAtFront_sll(struct sll_s *this, sll_node *node)
{
	if (!node)
		return -EINVAL;

	++(this->_size);
	node->next = this->head;

	/** SLL empty; Update tail */
	if (!this->head)
		this->tail = node;

	this->head = node;
	return 0;
}

int 
insertAtEnd_sll(struct sll_s *this, void *object)
{
	sll_node *tmp;

	tmp = this->newNode(object);
	if (!tmp)
		return -ENOMEM;

	return insertNodeAtEnd_sll(this, tmp);
}

int 
insertNodeAtEnd_sll(struct sll_s *this, sll_node *node)
{
	if (!node)
		return -EINVAL;

	++(this->_size);

	/** SLL Empty */
	if (!this->tail) {
		this->tail = this->head = node;
		return 0;
	}

	(this->tail)->next = node;
	this->tail = node;

	return 0;
}

int 
insertAfter_sll(struct sll_s *this, sll_node *node, void *object)
{
	sll_node *tmp;

	tmp = this->newNode(object);
	if (!tmp)
		return -ENOMEM;

	return insertNodeAfter_sll(this, node, tmp);
}

int 
insertNodeAfter_sll(struct sll_s *this, sll_node *node, sll_node *node_to_insert)
{
	if (!node || !node_to_insert)
		return -EINVAL;

	++(this->_size);
	node_to_insert->next = node->next;
	node->next = node_to_insert;

	return 0;
}

int 
insertBefore_sll(struct sll_s *this, sll_node *node, void *object)
{
	sll_node *tmp;

	tmp = this->newNode(object);
	if (!tmp)
		return -ENOMEM;

	return insertNodeBefore_sll(this, node, tmp);
}

/** Insert after node, and swap contents */
int
insertNodeBefore_sll(struct sll_s *this, sll_node *node, sll_node *node_to_insert)
{
	void *node_content;

	if (!node || !node_to_insert)
		return -EINVAL;

	++(this->_size);
	node_content = node->object;
	node->object = node_to_insert->object;
	node_to_insert->object = node_content;

	node_to_insert->next = node->next;
	node->next = node_to_insert;

	return 0;
}

