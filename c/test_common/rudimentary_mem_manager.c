#include <rudimentary_mem_manager.h>

/** 
 * Rudimentary Memory allocator/deallocator : 
 *  keep track of how many blocks are allocated and freed 
 *
 *  Used to test if a data structure's destroy/remove handler
 *  works well with a user-supplied memory manager
 */
int mem_blocks_counter = 0;

void *
allocator (int size)
{
	mem_blocks_counter++;

	return malloc(size);
}

void 
deallocator(void *ptr)
{
	mem_blocks_counter--;

	free(ptr);
}
