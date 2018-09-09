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

sll_node
sll::head(void)
{
	return sll_node(this->_sll.head);
}

sll_node
sll::tail(void)
{
	return sll_node(this->_sll.tail);
}
