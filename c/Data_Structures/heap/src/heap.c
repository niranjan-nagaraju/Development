#include <heap.h>

/** Uses up_heap to build heap */
void
build_heap (heap_t *heap)
{
	int i;

	/** 
	 * heap[(n-1)/2 .. 0] are non-leaf nodes 
	 * e.g.
	 *                  0
	 *               /     \
	 *              1       2 
	 *             / \     / \
	 *            3  4     5  6
	 */            

	for (i=(heap->_size-1)/2; i>=0; i--) {
		up_heap(heap, i);
	}

}

/** Make a heap starting at 'i', all the way up
 *  Assumes left and right subtrees are already heaps
 */  
void
up_heap (heap_t *heap, int i)
{
	int l, r;
	int largest;

	l = LEFT(i);
	r = RIGHT(i);

	if ( (l < heap->_size) && (heap->elements[l] > heap->elements[i]) ) 
		largest = l;
	else
		largest = i;

	if ( (r < heap->_size) && (heap->elements[r] > heap->elements[largest]) )
		largest = r;
	
	if (largest != i) {
		swapInt(&heap->elements[i], &heap->elements[largest]);
		up_heap(heap, largest);
	}
}

void
down_heap (heap_t *heap, int i)
{
}

void
heap_insert (heap_t *heap, int key)
{
}

int
heap_remove (heap_t *heap)
{
}

int
find_max (heap_t *heap)
{
}

int
extract_max (heap_t *heap)
{
}


