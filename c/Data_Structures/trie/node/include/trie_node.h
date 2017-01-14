#ifndef __TRIE_NODE_H__

#define __TRIE_NODE_H__

#include <stdlib.h>
#include <errno.h>
#include <boolean.h>

#define MAX_CHARS_IN_UNIVERSE 26
typedef struct trie_node_s {
	struct trie_node_s *children[MAX_CHARS_IN_UNIVERSE];

	/** Is there a word that ends in char at current node */
	boolean isEndOfWord; 

	/** 
	 * Running count of the number of words 
	 * that begin with the chars at current node 
	 */ 
	int prefix_count; 
} trie_node_t;


trie_node_t *trie_node_create(void);
void trie_node_delete (trie_node_t *node);

#endif //__TRIE_NODE_H__
