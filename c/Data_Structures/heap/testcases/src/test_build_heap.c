#include <heap.h>
#include <common.h>
#include <assert.h>

int main(void)
{
	heap_t heap;
	int i = 0;
	int l[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

	heap_init(&heap);

	for (i=0; i<10; i++) {
		heap.elements[i] = l[i];
	}
	heap._size = 10;

	heap_build(&heap);

	{
		int built_heap[] = {10, 9, 7, 8, 5, 6, 3, 1, 4, 2};
		assert(compareIntArrays(heap.elements, 0, built_heap, 0, 10) == TRUE);
	}

	return 0;

}
