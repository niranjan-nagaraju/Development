#include <sll.hpp>

sll::sll()
{
	sll_init(&(this->_sll));
}


sll::~sll() 
{
	sll_destroy(&(this->_sll), NULL);
} 

int
sll::length(void)
{
	return this->_sll._size;
}


/** Wrap SLL head into a sll_node object and return */
sll_node
sll::head(void)
{
	sll_node head;
	head.setNode(this->_sll.head);

	return head;
}

/** Wrap SLL tail into a sll_node object and return */
sll_node
sll::tail(void)
{
	sll_node tail;
	tail.setNode(this->_sll.tail);

	return tail;
}

