#include <sll_internal.h>
#include <common.h>
#include <stdlib.h>
#include <assert.h>
#include <test_common.h>

int main (void)
{
	sll_t sll;
	int i;

	sll_init (&sll);

	/** Insert 10 integers created by the memory allocator */
	for (i=0; i<10; i++) {
		int *tmp = allocator(sizeof(int));
		*tmp = i;
		sll_insert_at_front (&sll, (void *)tmp);
	}

	assert(sll._size == 10);

	/** verify what the memory allocator says about allocated blocks */
	assert(mem_blocks_counter == 10);

	sll_destroy(&sll, deallocator);

	assert(sll._size == 0);
	assert(sll.head == NULL);
	assert(sll.tail == NULL);

	/** verify what the memory allocator says about freed blocks */
	assert(mem_blocks_counter == 0);

	printf("SLL Destroy tests successful\n");
}
