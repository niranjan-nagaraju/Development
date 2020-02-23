#include <array_queue.h>
#include <common.h>
#include <assert.h>

#define MAX_QUEUE_SIZE 10

int main (void)
{
	array_queue_t myqueue;
	int i, rc;

	rc = arrayQ_init(&myqueue, MAX_QUEUE_SIZE);
	assert(rc == 0);

	assert(arrayQ_isEmpty(&myqueue) == TRUE);

	arrayQ_unsetCircular(&myqueue);
	assert(arrayQ_isCircular(&myqueue) == FALSE);

	arrayQ_print(&myqueue, printAsInt);
	for(i=0; i<MAX_QUEUE_SIZE; i++) {
		assert(arrayQ_enqueue(&myqueue, (void *)(i+1)) == 0);
		arrayQ_print(&myqueue, printAsInt);

		assert((int)arrayQ_peekFront(&myqueue) == 1);
		assert((int)arrayQ_peekRear(&myqueue) == (i+1));
		assert(arrayQ_len(&myqueue) == (i+1));
	}

	assert(arrayQ_isFull(&myqueue) == TRUE);
	
	for(i=0; i<MAX_QUEUE_SIZE-1; i++) {
		assert((int)arrayQ_peekFront(&myqueue) == i+1);
		assert((int)arrayQ_peekRear(&myqueue) == MAX_QUEUE_SIZE);
		assert(arrayQ_len(&myqueue) == (MAX_QUEUE_SIZE-i));

		assert(arrayQ_isFull(&myqueue) == TRUE);
		assert(arrayQ_dequeue(&myqueue) == (i+1));
		arrayQ_print(&myqueue, printAsInt);
	}

	/** Can't insert even though there's just 1 element in the Queue */
	assert(arrayQ_isFull(&myqueue) == TRUE);

	assert(arrayQ_enqueue(&myqueue, (void *)(11)) == -EINVAL);

	assert(arrayQ_dequeue(&myqueue) == (i+1));
	assert(arrayQ_isEmpty(&myqueue) == TRUE);

	printf("All tests succeed!\n");

	return 0;
}
