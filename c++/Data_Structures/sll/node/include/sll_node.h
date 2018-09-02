#ifndef _SLL_NODE_H_
#define _SLL_NODE_H_

class sll_node {
	private:
		void *data;
		sll_node *next;
	public:
		sll_node(void *data);
		~sll_node();
		//void print(printfn *printer);
};

#endif /** _SLL_NODE_H_ */

