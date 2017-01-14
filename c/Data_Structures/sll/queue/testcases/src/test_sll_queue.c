#include <queue.h>
#include <assert.h>

int main(void)
{
	queue_t queue;
	int i;

	queue_init(&queue);

	for (i=0; i<10; i++) {
		enqueue(&queue, (void *)i);
	}

	queue_print(&queue, printAsInt);

	for (i=0; i<10; i++) {
		assert((int)dequeue(&queue) == i);
	}
}
