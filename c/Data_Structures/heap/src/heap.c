#include <heap.h>

/** Make a heap starting at 'i', all the way up
 *  Assumes left and right subtrees are already heaps
 */  
void
max_heapify (heap_t *heap, int i)
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
		max_heapify(heap, largest);
	}
}


/**
 * Increase key value of node at {i} to 'key' 
 * and percolate up if the heap property is violated
 */
int
increase_key(heap_t *heap, int i, int key)
{
	/** New key is smaller than current key */
	if (key < heap->elements[i])
		return -EINVAL;

	heap->elements[i] = key;
	while( (i > 0) and (heap->elements[PARENT(i)] < heap->elements[i]) ) {
		swapInt(&heap->elements[i], &heap->elements[PARENT(i)]);
		i = PARENT(i);
	}

	return 0;
}


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
		max_heapify(heap, i);
	}

}

void
heap_insert (heap_t *heap, int key)
{
	int i;

	if (heap->_size == MAX_HEAP_SIZE) {
		return;
	}

	i = heap->size;

	heap->_size++;

	/** 
	 * Insert a smaller element at the bottom of the heap 
	 */
	heap->elements[i] = -1;

	/** Increase it's value to 'key' and percolate up */
	increase_key(heap, i, key);
}

int
find_max (heap_t *heap)
{
	return heap->elements[0];
}

int
extract_max (heap_t *heap)
{
	int max;

	if (heap->_size == 0)
		return -EINVAL;

	max = heap->elements[0];

	/** Move bottom most node to root */
	heap->elements[0] = heap->elements[heap->_size-1];
	heap->_size--;

	/** Heapify everything down below root */
	max_heapify(heap, 0);

	return max;
}


