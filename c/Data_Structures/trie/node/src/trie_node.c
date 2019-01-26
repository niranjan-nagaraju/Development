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
	int i = 0, j;

	if (!tmp) 
		return 0;

	memset(tmp, 0, sizeof(trie_node_t));

	for(i=0; i<MAX_CHARS_IN_UNIVERSE; i++) {
		tmp->items[i] = malloc(sizeof(trie_node_item_t));
		if (!tmp->items[i]) {
			/** 
			 * memory allocation failed mid-way
			 * free earlier allocations and return
			 */ 
			for(j=0; j<i; j++)
				free(tmp->items[i]);

			free(tmp);
			return 0;
		}
		memset(tmp->items[i], 0, sizeof(trie_node_item_t));
	}

	return tmp;
}



/**
 * Set values in trie node
 */
void
trie_node_set(trie_node_t *node, int key, boolean eow, int frequency)
{
	trie_node_item_t *item = node->items[key];

	item->key = key;

	item->isEndOfWord = eow;
	item->frequency = frequency;

	item->prefix_count = 10;
}



/** Deallocate specifed trie node, return data */
/** TODO: Extend to deallocate 'data' as well if deallocation method 
 *		for 'data' is specified
 */
void
trie_node_delete (trie_node_t *node)
{
	int i;

	if (!node)
		return;

	for(i=0; i<MAX_CHARS_IN_UNIVERSE; i++)
		free(node->items[i]);

	free(node);
}

