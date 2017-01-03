#include <heap.h>
#include <assert.h>

int main(void)
{
	heap_t heap;

	assert(heap_init(&heap) == 0);

	assert(heap._size == 0);
	assert(heap._len == 0);

	return 0;
}
