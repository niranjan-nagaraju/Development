#include <trie_node.h>
#include <stdlib.h>
#include <string.h>

/** 
 * Trie Node helper functions
 * No Error checking is performed here
 *  for 
 *   a) The functions are not expected to be called directly
 *   b) Trie is assumed to check for the boundary conditions, et, al.
 */
trie_node_t *
trie_node_create(void)
{
	trie_node_t *tmp = malloc(sizeof(trie_node_t));

	if (!tmp) 
		return 0;

	memset(tmp, 0, sizeof(trie_node_t));

	return tmp;
}



/** Deallocate specifed trie node, return data */
/** TODO: Extend to deallocate 'data' as well if deallocation method 
 *		for 'data' is specified
 */
void
trie_node_delete (trie_node_t *node)
{
	free(node);
}

