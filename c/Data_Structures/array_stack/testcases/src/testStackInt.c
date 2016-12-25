#include <array_stack.h>
#include <common.h>
#include <assert.h>

#define MAX_STACK_SIZE 10

int main (void)
{
	array_stack_t mystack;
	int i;

	assert(array_stack_init(&mystack, MAX_STACK_SIZE) == 0);

	assert(array_stack_isEmpty(&mystack) == TRUE);

	array_stack_print(&mystack, printAsInt);
	for(i=0; i<MAX_STACK_SIZE; i++) {
		assert(array_stack_push(&mystack, (void *)(i+1)) == 0);
		array_stack_print(&mystack, printAsInt);

		assert((int)array_stack_peek(&mystack) == (i+1));
		assert(array_stack_len(&mystack) == (i+1));
	}

	assert(array_stack_isFull(&mystack) == TRUE);
	assert(array_stack_isEmpty(&mystack) == FALSE);
	assert(array_stack_push(&mystack, (void *)(11)) == -EINVAL);
	
	for(i=0; i<MAX_STACK_SIZE; i++) {
		assert((int)array_stack_peek(&mystack) == (MAX_STACK_SIZE-i));
		assert(array_stack_len(&mystack) == (MAX_STACK_SIZE-i));

		assert(array_stack_pop(&mystack) == (MAX_STACK_SIZE-i));
		array_stack_print(&mystack, printAsInt);
	}

	assert(array_stack_isFull(&mystack) == FALSE);
	assert(array_stack_isEmpty(&mystack) == TRUE);

	array_stack_destroy(&mystack);

	printf("All tests succeed!\n");

	return 0;
}
