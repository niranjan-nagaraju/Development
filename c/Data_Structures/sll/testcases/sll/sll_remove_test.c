#include <sll.h>
#include <assert.h>
#include <stdio.h>
#include <common.h>

int main (void)
{
	sll_t sll;
	int i = 0;

	sll_init(&sll);

	/** Check graceful return for underflow conditions */
	i = (int)sll.remove_at_front(&sll);
	assert(i==0);
	assert(sll.length(&sll) == 0);
	assert((int)(sll.head) == 0);
	assert((int)(sll.tail) == 0);

	i = (int)sll.remove_at_end(&sll);
	assert(i==0);
	assert(sll.length(&sll) == 0);
	assert((int)(sll.head) == 0);
	assert((int)(sll.tail) == 0);

	i = (int)sll.remove_at_position(&sll, 1);
	assert(i==0);
	assert(sll.length(&sll) == 0);
	assert((int)(sll.head) == 0);
	assert((int)(sll.tail) == 0);

	/** Add a few numbers to the list */
	for (i=1; i<10; i++)
		sll.insert_at_end(&sll, (void *)i);

	i = (int)sll.remove_at_front(&sll);
	assert(i==1);
	assert(sll.length(&sll) == 8);
	assert((int)(sll.head->data) == 2);
	assert((int)(sll.tail->data) == 9);

	i = (int)sll.remove_at_front(&sll);
	assert(i==2);
	assert(sll.length(&sll) == 7);
	assert((int)(sll.head->data) == 3);
	assert((int)(sll.tail->data) == 9);

	i = (int)sll.remove_at_end(&sll);
	assert(i==9);
	assert(sll.length(&sll) == 6);
	assert((int)(sll.head->data) == 3);
	assert((int)(sll.tail->data) == 8);

	i = (int)sll.remove_at_end(&sll);
	assert(i==8);
	assert(sll.length(&sll) == 5);
	assert((int)(sll.head->data) == 3);
	assert((int)(sll.tail->data) == 7);

	i = (int)sll.remove_at_position(&sll, 0);
	assert(i==3);
	assert(sll.length(&sll) == 4);
	assert((int)(sll.head->data) == 4);
	assert((int)(sll.tail->data) == 7);

	i = (int)sll.remove_at_position(&sll, 3);
	assert(i==7);
	assert(sll.length(&sll) == 3);
	assert((int)(sll.head->data) == 4);
	assert((int)(sll.tail->data) == 6);

	/** Add some more */
	sll.insert_at_end(&sll, (void *)7);
	sll.insert_at_end(&sll, (void *)8);

	i = (int)sll.remove_at_position(&sll, 3);
	assert(i==7);
	assert(sll.length(&sll) == 4);
	assert((int)(sll.head->data) == 4);
	assert((int)(sll.tail->data) == 8);

	i = (int)sll.remove_at_position(&sll, 2);
	assert(i==6);
	assert(sll.length(&sll) == 3);
	assert((int)(sll.head->data) == 4);
	assert((int)(sll.tail->data) == 8);

	printf("SLL Remove tests successful\n");

	return 0;
}
