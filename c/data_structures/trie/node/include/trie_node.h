#ifndef __TRIE_NODE_H__

#define __TRIE_NODE_H__

#include <stdlib.h>
#include <errno.h>
#include <boolean.h>

typedef struct trie_node_s trie_node_t;


/**
 * What goes into a trie node
 *   children - A trie node pointer for the next level in the trie,
 *         for words starting with the prefix at current item
 *   isEndOfWord - end of word status/just a prefix
 *   frequency - frequency of occurence of a certain word
 *   prefix_count - number of words with the prefix at current node
 */  
typedef struct trie_node_item_s {
	/** child nodes for the characters following prefix at current node */
	trie_node_t *children;

	/** Is there a word that ends in char at current node */
	boolean isEndOfWord;

	/** 
	 * Word frequency - number of times it was added to the trie
	 * while the word itself is stored only once, this keeps track of how 
	 * many times 'addWord()' was called with the current word 
	 * ending in current node.
	 *
	 * Valid only if EoW(end of word) is true.
	 *
	 * e.g, addWord('abc') twice should make node item at 'c' in the trie path,
	 * the EoW, and a frequency of 2
	 */
	int frequency;

	/** 
	 * Running count of the number of words 
	 * that begin with the (sequence of chars) at current node 
	 */ 
	int prefix_count; 
} trie_node_item_t;

#define MAX_CHARS_IN_UNIVERSE 128 /** Printable ascii characters */
struct trie_node_s {
	trie_node_item_t *items[MAX_CHARS_IN_UNIVERSE];
	int num_items; // number of valid characters in this particular trie node
};


trie_node_t *trie_node_create(void);
void trie_node_delete (trie_node_t *node);

void trie_node_setChildNode(trie_node_t *parent, char key, trie_node_t *child);
trie_node_t * trie_node_getChildNode(trie_node_t *parent, char key);

int trie_node_add(trie_node_t *node, char key);
void trie_node_remove(trie_node_t *node, char key);
void trie_node_setEndOfWord(trie_node_t *node, char key, boolean eow);
boolean trie_node_getEndOfWord(trie_node_t *node, char key);
void trie_node_setFrequency(trie_node_t *node, char key, int frequency);
int trie_node_getFrequency(trie_node_t *node, char key);
void trie_node_setPrefix_count(trie_node_t *node, char key, int prefix_count);
int trie_node_getPrefix_count(trie_node_t *node, char key);
#endif //__TRIE_NODE_H__
