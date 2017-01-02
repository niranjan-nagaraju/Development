#include <heap.h>
#include <assert.h>

int main(void)
{
	int l[] = {1,2,3,4,5,6,7,8,9,10};
	int i;
	heap_t heap1, heap2;

	heap_init(&heap1);

	for (i=0; i<sizeof(l)/sizeof(int); i++) {
		heap_insert(&heap1, l[i]);
		assert(heap1._size == i+1);
		printIntArray(heap1.elements, heap1._size);
	}

	heap_init(&heap2);

	/** A reverse sorted list is already a heap */ 
	for (i=sizeof(l)/sizeof(int)-1; i>=0; i--) {
		int lr[] = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
		heap_insert(&heap2, l[i]);
		assert(heap2._size == 10-i);
		assert(compareIntArrays(heap2.elements, 0, lr, 0, heap2._size));
	}

	return 0;
}
