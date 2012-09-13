#include <sll.h>
#include <common.h>
#include <assert.h>

struct test_struct {
	int tsi;
	char tsc;
};

int main(void)
{
	sll_t sll;
	int i;

	sll_node_t *node;

	sll_init(&sll);

	for (i=1; i<11; i++) {
		sll_insert_at_front (&sll, (void *)i);
	}

	node = sll_find_node (&sll, (void *)5, compareInts);
	assert (node != NULL);
	assert ((int)(node->data) == 5);

	node = sll_find_node (&sll, (void *)1, compareInts);
	assert (node != NULL);
	assert ((int)(node->data) == 1);

	node = sll_find_node (&sll, (void *)10, compareInts);
	assert (node != NULL);
	assert ((int)(node->data) == 10);

	node = sll_find_node (&sll, (void *)11, compareInts);
	assert (node == NULL);


	assert((int)(sll_find (&sll, (void *)5, compareInts)) == 5);
	assert((int)(sll_find (&sll, (void *)1, compareInts)) == 1);
	assert((int)(sll_find (&sll, (void *)10, compareInts)) == 10);
	assert((int)(sll_find (&sll, (void *)11, compareInts)) == 0);

	sll_destroy(&sll, NULL);

	printf("SLL Find tests successful\n");

	return 0;
}
