#include <sll_node.h>
#include <common.h>

/** Don't worry about freeing just yet :) */

struct test_struct {
	int tsi;
	char tsc;
};

void printStruct (void *object)
{
	struct test_struct *obj = (struct test_struct *) object;
	printf("%d %c", obj->tsi, obj->tsc);
}

int
main (void)
{
	sll_node_t *test_node = NULL;
	int i = 10;
	char c = 'a';

	struct test_struct test_obj = {42, 'Z'};

	test_node = sll_node_create ((void *)i);
	sll_node_print (test_node, printAsInt);
	printNL();
		
	test_node = sll_node_create ((void *) c);
	sll_node_print (test_node, printAsChar);
	printNL();

	test_node = sll_node_create ((void *) &test_obj);
	sll_node_print (test_node, printStruct);
	printNL();

	return 0;
}
