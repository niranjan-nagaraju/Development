#ifndef _SLL_NODE_HPP_
#define _SLL_NODE_HPP_

extern "C" {
	#include "sll_node.h"
}

class sll; // forward declaration to be defined as a friend class of sll node
class NodeUT; // forward declaration for Unit testcases

class sll_node {
	private:
		sll_node_t *_node;
		void setNode(sll_node_t *n); /** Directly assign _node from an sll_node_t *object */
	public:
		sll_node(void *data=NULL);
		~sll_node();
		void *get();
		void set(void *data);
		sll_node next();
		void *destroy();
		//void print(printfn *printer);

		// Friend classes
		friend class sll;
		friend class NodeUT;
};

#endif /** _SLL_NODE_HPP_ */

