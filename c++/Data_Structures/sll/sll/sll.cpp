#include <sll.h>
using namespace std;

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

