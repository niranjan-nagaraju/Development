#include <array_queue.h>
#include <common.h>

/** Placeholder for now */

int main (void)
{
	array_queue_t myqueue;
	int i;

	(void)arrayQ_init(&myqueue, 10);

	for(i=0; i<10; i++) {
		printf("Adding %d to queue\n", i+1);
		myqueue.enqueue(&myqueue, (void *)(i+1));
		myqueue.print(&myqueue, printAsInt);
	}
	
	for(i=0; i<10; i++) {
		printf("Removing %d from queue\n", (int)myqueue.dequeue(&myqueue));
		myqueue.print(&myqueue, printAsInt);
	}

	return 0;
}
