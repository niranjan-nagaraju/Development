#ifndef _SLL_CPP_H_
#define _SLL_CPP_H_

extern "C" {
	#include "../../../c/Data_Structures/sll/sll/include/sll.h"
	#include "../../../c/Data_Structures/sll/sll/include/sll_internal.h"
}

class sll{
private:
	sll_t _sll;
public:
	sll();
	~sll();

	int length(void); 

	/** Insert Operations */
	int insert_at_front (void *data);
    int insert_at_end (void *data);
    int insert_at_position (void *data, int pos);
    int insert_after (void *data, void *key, comparefn compare);
    int insert_node_at_front (sll_node_t *node);
    int insert_node_at_end (sll_node_t *node);
    int insert_node_at_position (sll_node_t *node, int pos);
    void insert_after_node (sll_node_t *node, sll_node_t *new_node);
};

#endif // _SLL_CPP_H_
