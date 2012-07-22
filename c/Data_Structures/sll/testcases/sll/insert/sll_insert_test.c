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
	int i = 10;
	char c = 'a';

	sll_node_t *node;

	sll_init(&sll);

	sll_insert_at_front(&sll, (void *)20);
	sll_insert_at_front(&sll, (void *)10);
	sll_insert_at_front(&sll, (void *)30);

	sll_insert_at_end(&sll, (void *) 100);
	
	sll_insert_at_position(&sll, (void *) 120, 2);
	sll_insert_at_position(&sll, (void *) 101, 15);
	sll_insert_at_position(&sll, (void *) 122, 5);

	sll_insert_at_end(&sll, (void *) 140);
	sll_insert_at_position(&sll, (void *) 150, 7);

	assert ((int)(sll.head->data) == 30);
	assert ((int)(sll.tail->data) == 150);

	node = sll.head->next;
	assert ((int)(node->data) == 10);

	node = node->next;
	assert ((int)(node->data) == 120);

	node = node->next;
	assert ((int)(node->data) == 20);

	node = node->next;
	assert ((int)(node->data) == 100);

	node = node->next;
	assert ((int)(node->data) == 122);

	node = node->next;
	assert ((int)(node->data) == 140);
	
	node = node->next;
	assert ((int)(node->data) == 150);

	return 0;
}
