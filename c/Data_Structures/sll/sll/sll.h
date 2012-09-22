#ifndef _SLL_H_
#define _SLL_H_

#include <sll_node.h>
#include <common.h>

typedef struct sll_s {
	sll_node_t *head;
	sll_node_t *tail;
	int _size;
} sll_t;

#include <sll_find.h>
#include <sll_insert.h>
#include <sll_print.h>
#include <sll_remove.h>

#include <errno.h>

/** Core operations */
void sll_init (sll_t *sll);
int sll_length (sll_t *sll);
void sll_destroy (sll_t *sll, deallocatorfn deallocate);

#endif // _SLL_H_
