#include <heap.h>
#include <assert.h>

int main(void)
{
	heap_t heap;
	int i = 0;
	int *l = heap.elements;

	for (i=0; i<8; i++) {
		heap.elements[i] = (i+1);
	}

	for(i=0; i<3; i++) {
		assert(l[LEFT(i)] == 2*l[i]);
		assert(l[RIGHT(i)] == 2*l[i]+1);
	}

	/** Leaf nodes */
	for (i=6; i>2; i--) {
		assert(l[PARENT(i)] == l[i]/2);
	}
	return 0;
}
