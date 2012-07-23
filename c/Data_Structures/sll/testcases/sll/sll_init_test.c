#include <sll.h>
#include <assert.h>
#include <stdio.h>

int main (void)
{
	sll_t sll;

	sll_init(&sll);

	assert(sll.head == NULL);
	assert(sll.tail == NULL);
	assert(sll._size == 0);

	assert(sll_length(&sll) == 0);

	printf("SLL Init tests successful\n");

	return 0;
}
