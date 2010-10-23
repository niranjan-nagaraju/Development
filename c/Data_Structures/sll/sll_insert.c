#include <sll.h>

static int _insertNodeAt_sll(sll_t *this, sll_node *node, int pos);

/** No Validation for node or pos */
static int 
_insertNodeAt_sll(sll_t *this, sll_node *node, int pos)
{
	sll_node *head = this->head;
	int i = 1;
	int orig_size;

	orig_size = (this->_size)++;

	if (pos == 0 || !head) {
		node->next = head;
		this->head = node;

		if (!head)
			this->tail = node;

		return 0;
	}
	
	/** position exceeds curr range; add at end */
	if (orig_size < pos) {
		this->tail->next = node;
		this->tail = node;

		return 0;
	}

	while (i++ < pos)
		head = head->next;

	node->next = head->next;
	head->next = node;

	return 0;
}

int 
insertAt_sll(struct sll_s *this, void *object, int pos)
{
	sll_node *tmp;

	if (pos < 0)
		return -EINVAL;

	tmp = this->newNode(object);
	if (!tmp)
		return -ENOMEM;

	return _insertNodeAt_sll(this, tmp, pos);
}

int 
insertNodeAt_sll(struct sll_s *this, sll_node *node, int pos)
{
	if (pos < 0 || !node)
		return -EINVAL;

	return _insertNodeAt_sll(this, node, pos);
}

int 
insertAtRev_sll(struct sll_s *this, void *object, int pos)
{
	return 0;
}

int 
insertNodeAtRev_sll(struct sll_s *this, sll_node *node, int pos)
{
	return 0;
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

