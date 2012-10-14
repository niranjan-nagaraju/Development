#ifndef _SLL_CPP_H_
#define _SLL_CPP_H_

extern "C" {
	#include "../../../c/Data_Structures/sll/sll/sll.h"
//	#include "../../../c/Data_Structures/sll/sll/sll_internal.h"
}

class sll{
private:
	sll_t _sll;
public:
	sll() {
		sll_init(&(this->_sll));
	}

	int length(void); 
};

#endif // _SLL_H_
