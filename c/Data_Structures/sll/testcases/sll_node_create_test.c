#include <sll_node.h>
#include <assert.h>

/** Don't worry about freeing just yet :) */

int
main (void)
{
	sll_node_t *test_node = NULL;
	int i = 10;
	char c = 'a';
	struct test_struct {
		int tsi;
		char tsc;
	};

	struct test_struct test_obj = {42, 'Z'};
	struct test_struct *tsptr;

	test_node = sll_node_create ((void *)i);
	i = (int) test_node->data;
	assert(10 == i);
	
	test_node = sll_node_create ((void *) c);
	assert('a' == (char) test_node->data);

	test_node = sll_node_create ((void *) &test_obj);
	tsptr = (struct test_struct *) test_node->data;	
	assert(42 == tsptr->tsi &&  'Z' == tsptr->tsc);

	return 0;
}
