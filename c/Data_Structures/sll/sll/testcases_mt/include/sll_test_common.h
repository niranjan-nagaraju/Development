#ifndef __SLL_TEST_COMMON_H_
#define __SLL_TEST_COMMON_H_

#include <sll.h>
#include <common.h>
#include <assert.h>
#include <test_common.h>

void verify_list_against_sll (sll_t *sll, void *test_list[], int test_list_len, comparefn compare);

#endif
