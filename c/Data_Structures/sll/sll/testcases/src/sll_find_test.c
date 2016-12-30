#include <sll.h>
#include <common.h>
#include <assert.h>
#include <test_common.h>

/** Simple find, direct integer compare */
void 
test_int_sll_find(void)
{
	sll_t sll = SLL_INITIALIZER;
	int i;
	sll_node_t *node;

	for (i=1; i<11; i++) {
		sll_insert_at_front (&sll, (void *)i);
	}

	node = sll_find_containing_node (&sll, (void *)5, compareInts);
	assert (node != NULL);
	assert ((int)(node->data) == 5);

	node = sll_find_containing_node (&sll, (void *)1, compareInts);
	assert (node != NULL);
	assert ((int)(node->data) == 1);

	node = sll_find_containing_node (&sll, (void *)10, compareInts);
	assert (node != NULL);
	assert ((int)(node->data) == 10);

	node = sll_find_containing_node (&sll, (void *)11, compareInts);
	assert (node == NULL);


	assert((int)(sll_find (&sll, (void *)5, compareInts)) == 5);
	assert((int)(sll_find (&sll, (void *)1, compareInts)) == 1);
	assert((int)(sll_find (&sll, (void *)10, compareInts)) == 10);
	assert((int)(sll_find (&sll, (void *)11, compareInts)) == 0);

	sll_destroy(&sll, NULL);
}


/** Find based on keys in a structure */
void 
test_keys_find_in_sll (void)
{
	struct test_struct objs[10], *ptr;
	sll_t sll;
	int i;
	sll_node_t *node;

	sll_init(&sll);

	for (i=1; i<11; i++) {
		objs[i-1].tsi = i*i; // 1, 4, 9, 16, 25, 36, 49, 64, 81, 100
		objs[i-1].tsc = (char)('A' + i+i); // 'C', 'E', 'G', 'I', 'K', 'M', 'O', 'Q', 'S', 'U'
		sll_insert_at_front (&sll, (void *)(objs+i));
	}
	
	node = sll_find_containing_node (&sll, (void *)4, compareIntKey);
	assert (node != NULL);
	ptr = node->data;
	assert (ptr->tsi == 4);
	assert (ptr->tsc == 'E');

	node = sll_find_containing_node (&sll, (void *)11, compareIntKey);
	assert (node == NULL);

	node = sll_find_containing_node (&sll, (void *)'G', compareCharKey);
	assert (node != NULL);
	ptr = node->data;
	assert (ptr->tsi == 9);
	assert (ptr->tsc == 'G');

	node = sll_find_containing_node (&sll, (void *)'Z', compareCharKey);
	assert (node == NULL);

	ptr = sll_find(&sll, (void *)36, compareIntKey);
	assert (ptr != NULL);
	assert (ptr->tsi == 36);
	assert (ptr->tsc == 'M');

	ptr = sll_find(&sll, (void *)26, compareIntKey);
	assert (ptr == NULL);

	ptr = sll_find(&sll, (void *)'I', compareCharKey);
	assert (ptr != NULL);
	assert (ptr->tsi == 16);
	assert (ptr->tsc == 'I');

	ptr = sll_find(&sll, (void *)'Z', compareCharKey);
	assert (ptr == NULL);

	sll_destroy(&sll, NULL);
}

int 
main (void)
{
	test_int_sll_find();
	test_keys_find_in_sll();
	
	printf("SLL Find tests successful\n");

	return 0;
}
