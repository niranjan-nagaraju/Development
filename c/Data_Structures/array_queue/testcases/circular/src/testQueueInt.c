#include <array_queue.h>
#include <common.h>
#include <assert.h>

int main (void)
{
	array_queue_t myqueue;
	int i, rc;

	rc = arrayQ_init(&myqueue, 10);
	assert(rc == 0);

	assert(arrayQ_isEmpty(&myqueue) == TRUE);

	/** Default configuration is a circular queue */
	assert(arrayQ_isCircular(&myqueue) == TRUE);

	arrayQ_setCircular(&myqueue);
	assert(arrayQ_isCircular(&myqueue) == TRUE);

	for(i=0; i<10; i++) {
		assert(arrayQ_enqueue(&myqueue, (void *)(i+1)) == 0);
	
		arrayQ_print(&myqueue, printAsInt);
#if 0
		assert((int)arrayQ_peekFront(&myqueue) == 1);
		assert((int)arrayQ_peekRear(&myqueue) == (i+1));
		assert(arrayQ_len(&myqueue) == (i+1));
#endif
	}


#if 0
	assert(arrayQ_isFull(&myqueue) == TRUE);
	
	for(i=0; i<10; i++) {
		assert((int)arrayQ_peekFront(&myqueue) == i+1);
		assert((int)arrayQ_peekRear(&myqueue) == 10);
		assert(arrayQ_len(&myqueue) == (10-i));

		assert(arrayQ_dequeue(&myqueue) == (i+1));
	}

	assert(arrayQ_isEmpty(&myqueue) == TRUE);
#endif

	printf("All tests succeed!\n");

	return 0;
}
