#include <sll_node.h>
#include <stdio.h>

int
main (void)
{
	sll_node_t *test_node = NULL;
	int i = 10;
	char c = 'a';
	struct test_struct {
		int tsi;
		float tsf;
	};
	
	struct test_struct test_obj = {42, 4.242};
	struct test_struct *tsptr;

	test_node = sll_node_create ((void *) i);
	printf ("%p\n", test_node);
	if (test_node == NULL)
		printf("NULL\n");

	i = (int) test_node->data;
	printf ("%d\n", i);
	
	test_node = sll_node_create ((void *) c);
	printf ("%p\n", test_node);
	printf ("%c\n", (char) test_node->data);

	test_node = sll_node_create ((void *) &test_obj);
	tsptr = (struct test_struct *) test_node->data;	
	printf ("%d %f\n", tsptr->tsi, tsptr->tsf);

	return 0;
}
