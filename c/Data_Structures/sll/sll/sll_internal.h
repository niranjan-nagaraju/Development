#ifndef _SLL_INTERNAL_H_
#define _SLL_INTERNAL_H_

#include <sll.h>

#include <sll_find.h>
#include <sll_insert.h>
#include <sll_print.h>
#include <sll_remove.h>

#include <errno.h>

/** Core operations */
int sll_length (sll_t *sll);
void sll_destroy (sll_t *sll, deallocatorfn deallocate);
#endif
