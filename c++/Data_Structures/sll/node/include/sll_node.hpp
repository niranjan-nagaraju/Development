#ifndef _SLL_NODE_HPP_
#define _SLL_NODE_HPP_

extern "C" {
	#include "sll_node.h"
}

class sll_node {
	private:
		sll_node_t *_node;
	public:
		sll_node(void *data);
		~sll_node();
		void *get();
		//void print(printfn *printer);
};

#endif /** _SLL_NODE_HPP_ */

