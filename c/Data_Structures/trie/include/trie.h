#ifndef __TRIE_H__
#define __TRIE_H__

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


typedef struct trie_s {
	trie_node_t *root;

	/** Number of words in the trie */
	int numWords;	
}trie_t;


/** Create a new node */
trie_node_t *createNode(void);

int trie_addWord(trie_t *trie, const char *word);
boolean trie_hasWord(trie_t *trie, const char *word);
boolean trie_hasPrefix(trie_t *trie, const char *prefix);
int trie_findPrefixesCount(trie_t *trie, const char *prefix);

/** Return words that begin with the specified prefix */
int trie_findPrefixMatches(trie_t *trie, const char *prefix, char **matches);

#endif /** __TRIE_H__ */
