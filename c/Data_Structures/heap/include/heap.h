#ifndef __HEAP_H__
#define __HEAP_H__

/** Parent: (i-1)/2, Indexing starts at 0 */
#define PARENT(i) ((i)-1) >> 1
	
/** Left child: 2i + 1 */
#define LEFT(i) ((i)<<1) + 1
	
/** Right child: 2i + 2 */
#define RIGHT(i) ((i)<<1) + 2
	

/** Max-heap, static array to begin with */
typedef struct heap_s {
#define MAX_HEAP_SIZE 100
	int elements[MAX_HEAP_SIZE];
	int _size;
} heap_t;

void heap_init(heap_t *heap);
void heapify_down (heap_t *heap, int i);
void heapify_up (heap_t *heap, int i);

void heap_insert (heap_t *heap, int key);
int heap_increase_key(heap_t *heap, int i, int key);

int heap_find_max (heap_t *heap);
int heap_extract_max (heap_t *heap);

#endif /** __HEAP_H__ */
