#ifndef _SLL_HPP_
#define _SLL_HPP_

extern "C" {
	#include "sll.h"
	#include "sll_internal.h"
}

#include <sll_node.hpp>

class sll{
private:
	sll_t _sll;
public:
	sll();
	~sll();

	int length(void);
	sll_node head(void);
	sll_node tail(void);

	/** Insert Operations */
	int insert_at_front (void *data);
    int insert_at_end (void *data);
    int insert_at_position (void *data, int pos);
    //int insert_after (void *data, void *key, comparefn compare);
    int insert_node_at_front (sll_node node);
    int insert_node_at_end (sll_node node);
    int insert_node_at_position (sll_node node, int pos);
    void insert_after_node (sll_node node, sll_node new_node);
};

#endif // _SLL_HPP_
