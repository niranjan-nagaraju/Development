#ifndef _SLL_H_
#define _SLL_H_

#include <sll_node.h>
#include <common.h>

typedef struct sll_s {
	sll_node_t *head;
	sll_node_t *tail;
	int _size;
} sll_t;

/** The only public interface that can be called outside of sll object */
void sll_init (sll_t *sll);

#endif // _SLL_H_
