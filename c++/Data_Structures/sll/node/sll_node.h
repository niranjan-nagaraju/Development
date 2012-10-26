#ifndef _SLL_CPP_NODE_H_
#define _SLL_CPP_NODE_H_

extern "C" {
	#include "../../../../c/Data_Structures/sll/node/sll_node.h"
}

class sll_node {
	private:
		sll_node_t _node;
	public:
		sll_node(void *data=NULL);
		~sll_node();
		void print(printfn *printer);
}

#endif #_SLL_CPP_NODE_H_

