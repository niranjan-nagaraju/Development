#include <heap.h>
#include <common.h>
#include <assert.h>

int main(void)
{
	heap_t heap;
	int i = 0;
	int l[] = {16, 4, 10, 14, 7, 9, 3, 2, 8, 1};

	for (i=0; i<10; i++) {
		heap.elements[i] = l[i];
	}
	heap._size = 10;

	printf("Before up heap\n");
	printIntArray(heap.elements, 10);

	max_heapify(&heap, 1);

	printf("After up heap\n");
	printIntArray(heap.elements, 10);

	{
		int m[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
		assert(compareIntArrays(heap.elements, 0, m, 0, 10) == TRUE);
	}

	return 0;
}
