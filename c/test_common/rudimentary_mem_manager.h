#ifndef __RUDIMENTARY_MEMORY_ALLOCATOR_H_
#define __RUDIMENTARY_MEMORY_ALLOCATOR_H_ 

#include <stdlib.h>

/** Rudimentary memory allocator/deallocator */
extern int mem_blocks_counter;
void *allocator (int size);
void deallocator(void *ptr);

#endif
