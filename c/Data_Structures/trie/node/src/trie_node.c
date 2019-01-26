#include <trie_node.h>
#include <stdlib.h>
#include <string.h>


/**
 * Create an empty trie node and return
 */
trie_node_t *
trie_node_create(void)
{
	trie_node_t *tmp = malloc(sizeof(trie_node_t));
	int i = 0, j;

	if (!tmp) 
		return 0;

	memset(tmp, 0, sizeof(trie_node_t));

	return tmp;
}



/**
 * Add a specified character into the trie node
 */
int
trie_node_add(trie_node_t *node, char key)
{
	/** 
	 * key is supposed to be between 1-127, 0 is the NUL character, 
	 * 127 is DELETE in ascii
	 *
	 * If key already 'exists' in the node, return without doing anything
	 */
	if( !(key > 0) )
		return -EINVAL;

	trie_node_item_t *item = node->items[key];

	/** node item already exists for character-key */
	if (item)
		return 0;
	
	node->items[key] = malloc(sizeof(trie_node_item_t));
	if (!node->items[key])
		return -ENOMEM; /** malloc() failed */

	memset(node->items[key], 0, sizeof(trie_node_item_t));
	node->num_items++; // new character added to the trie node

	return 0;
}



/**
 * Set EoW status in trie node for a specific 'key' in the node 
 */
void
trie_node_setEndOfWord(trie_node_t *node, char key, boolean eow)
{
	trie_node_item_t *item = node->items[key];
	item->isEndOfWord = eow;
}


/**
 * Set frequency status in trie node for a specific 'key' in the node 
 */
void
trie_node_setFrequency(trie_node_t *node, char key, int frequency)
{
	trie_node_item_t *item = node->items[key];
	item->frequency = frequency;
}

/**
 * Set prefix count in trie node for a specific 'key' in the node 
 */
void
trie_node_setPrefix_count(trie_node_t *node, char key, int prefix_count)
{
	trie_node_item_t *item = node->items[key];
	item->prefix_count = prefix_count;
}


/**
 * remove a character/key from a node
 */ 
void
trie_node_remove(trie_node_t *node, char key)
{
	/** Nothing to do */
	if(!node->items[key])
		return;

	free(node->items[key]);
	node->items[key] = 0;
	node->num_items--;
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

