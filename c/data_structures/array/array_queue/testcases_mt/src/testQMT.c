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
		arrayQ_enqueue(&myqueue, (void *)(i+1));
		arrayQ_print(&myqueue, printAsInt);
	}
	
	for(i=0; i<10; i++) {
		printf("Removing %d from queue\n", (int)arrayQ_dequeue(&myqueue));
		arrayQ_print(&myqueue, printAsInt);
	}

	return 0;
}
