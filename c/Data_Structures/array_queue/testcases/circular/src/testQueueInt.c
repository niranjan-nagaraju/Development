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

	/** Default configuration is a circular queue */
	assert(arrayQ_isCircular(&myqueue) == TRUE);

	arrayQ_setCircular(&myqueue);
	assert(arrayQ_isCircular(&myqueue) == TRUE);

	arrayQ_print(&myqueue, printAsInt);
	for(i=0; i<MAX_QUEUE_SIZE; i++) {
		assert(arrayQ_enqueue(&myqueue, (void *)(i+1)) == 0);
	
		arrayQ_print(&myqueue, printAsInt);

		assert(arrayQ_len(&myqueue) == (i+1));
		assert((int)arrayQ_peekFront(&myqueue) == 1);
		assert((int)arrayQ_peekRear(&myqueue) == (i+1));
	}


	assert(arrayQ_isFull(&myqueue) == TRUE);
	assert(arrayQ_enqueue(&myqueue, (void *)(11)) == -EINVAL);
	
	for(i=0; i<MAX_QUEUE_SIZE-1; i++) {
		assert((int)arrayQ_peekFront(&myqueue) == i+1);
		assert((int)arrayQ_peekRear(&myqueue) == 10);
		assert(arrayQ_len(&myqueue) == (10-i));

		assert(arrayQ_dequeue(&myqueue) == (i+1));
		arrayQ_print(&myqueue, printAsInt);
	}

	/** Can insert after (n-1) dequeues in a circular queue */
	assert(arrayQ_isFull(&myqueue) == FALSE);
	assert(arrayQ_enqueue(&myqueue, (void *)(12)) == 0);

	//arrayQ_destroy(&myqueue);
	//assert(arrayQ_isEmpty(&myqueue) == TRUE);

	printf("All tests succeed!\n");

	return 0;
}
