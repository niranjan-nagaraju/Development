#include <sll.h>

sll::sll()
{
	sll_init(&(this->_sll));
}


sll::~sll() 
{
	sll_destroy(&(this->_sll), NULL);
} 

int sll::length(void)
{
	return this->_sll._size;
}

