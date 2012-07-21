#include <bst_node.h>
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
	bst_node_t *test_node = NULL;
	int i = 10;
	char c = 'a';

	struct test_struct test_obj = {42, 'Z'};
	struct test_struct *tsptr;

	test_node = bst_node_create ((void *)i);
	bst_node_print (test_node, printAsInt);
	printNL();
		
	test_node = bst_node_create ((void *) c);
	bst_node_print (test_node, printAsChar);
	printNL();

	test_node = bst_node_create ((void *) &test_obj);
	bst_node_print (test_node, printStruct);
	printNL();

	return 0;
}
