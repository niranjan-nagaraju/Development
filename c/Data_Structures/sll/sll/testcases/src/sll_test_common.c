#include <sll_test_common.h>

/** Verify the elements in SLL against a list of elements 
 *    Used to validate SLL operations
 */
void
verify_list_against_sll (sll_t *sll, void *test_list[], int test_list_len, comparefn compare)
{
	int i;
	sll_node_t *node = sll->head;

	for ( i=0; i < test_list_len; i++ ) {
		assert (compare(test_list[i], -1, node->data, -1) == 0);
		node = node->next;
	}
}

