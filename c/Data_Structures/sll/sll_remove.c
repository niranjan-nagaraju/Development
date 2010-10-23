#include <sll.h>

void *removeObject_sll(struct sll_s *this, void *object)
{
	return NULL;
}

sll_node *removeNode_sll(struct sll_s *this, sll_node *node)
{
	sll_node *head = this->head;

	if (!node || !head)
		return NULL;

	while (head->next && head->next != node) 
		head = head->next;

	/** Couldn't locate 'node' */
	if (head == this->tail)
		return NULL;

	--(this->_size);
	
	/** head->next is the node immediately b4 the node to be deleted */

	/** node to be deleted is the tail node; Update tail to prev node */
	if (node == this->tail)
		this->tail = head->next;

	/** Execute the delete */
	head->next = node->next;

	return node;
}

void *removeAt_sll(struct sll_s *this, int pos)
{
	return NULL;
}

sll_node *removeNodeAt_sll(struct sll_s *this, int pos)
{
	return NULL;
}

void *removeAtRev_sll(struct sll_s *this, int pos)
{
	return NULL;
}

sll_node *removeNodeAtRev_sll(struct sll_s *this, int pos)
{
	return NULL;
}

void *removeAfter_sll(struct sll_s *this, sll_node *node)
{
	return NULL;
}

sll_node *removeNodeAfter_sll(struct sll_s *this, sll_node *node)
{
	return NULL;
}

void *removeBefore_sll(struct sll_s *this, sll_node *node)
{
	return NULL;
}

sll_node *removeNodeBefore_sll(struct sll_s *this, sll_node *node)
{
	return NULL;
}

void *removeFirst_sll(struct sll_s *this)
{
	return NULL;
}

sll_node *removeFirstNode_sll(struct sll_s *this)
{
	return NULL;
}


void *removeLast_sll(struct sll_s *this)
{
	return NULL;
}

sll_node *removeLastNode_sll(struct sll_s *this)
{
	return NULL;
}
