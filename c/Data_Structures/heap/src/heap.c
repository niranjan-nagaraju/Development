#include <heap.h>
#include <common.h>
#include <errno.h>

void
heap_init(heap_t *heap)
{
	heap->_size = 0;
}

/** Make a heap starting at node 'i', all the way down
 *  Assumes left and right subtrees are already heaps
 */  
void
heapify_down (heap_t *heap, int i)
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
		heapify_down(heap, largest);
	}
}

/** 
 * Heapify all the nodes up, starting at node, 'i'
 */ 
void
heapify_up(heap_t *heap, int i)
{
	while( (i > 0) && (heap->elements[PARENT(i)] < heap->elements[i]) ) {
		swapInt(&heap->elements[i], &heap->elements[PARENT(i)]);
		i = PARENT(i);
	}
}

/**
 * Increase key value of node at {i} to 'key' 
 * and percolate up if the heap property is violated
 */
int
heap_increase_key(heap_t *heap, int i, int key)
{
	/** New key is smaller than current key */
	if (key < heap->elements[i])
		return -EINVAL;

	heap->elements[i] = key;
	heapify_up(heap, i);

	return 0;
}


/** 
 * Uses heapify_down to build heap starting from 
 * first non-leaf node from the bottom 
 */
void
heap_build (heap_t *heap)
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
		heapify_down(heap, i);
	}

}

void
heap_insert (heap_t *heap, int key)
{
	int i;

	if (heap->_size == MAX_HEAP_SIZE) {
		return;
	}

	i = heap->_size;

	heap->_size++;

	/** 
	 * Insert a smaller element at the bottom of the heap 
	 */
	heap->elements[i] = -1;

	/** Increase it's value to 'key' and percolate up */
	heap_increase_key(heap, i, key);
}

int
heap_find_max (heap_t *heap)
{
	if (heap->_size == 0)
		return -EINVAL;

	return heap->elements[0];
}

int
heap_extract_max (heap_t *heap)
{
	int max;

	if (heap->_size == 0)
		return -EINVAL;

	max = heap->elements[0];

	/** Move bottom most node to root */
	heap->elements[0] = heap->elements[heap->_size-1];
	heap->_size--;

	/** Heapify everything down below root */
	heapify_down(heap, 0);

	return max;
}


