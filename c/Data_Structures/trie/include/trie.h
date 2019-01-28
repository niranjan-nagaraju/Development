#ifndef __TRIE_H__
#define __TRIE_H__

#include <boolean.h>
#include <trie_node.h>

#include <queue.h>

typedef struct trie_s {
	trie_node_t *root;

	/** Number of words in the trie */
	int numWords;	
}trie_t;


void trie_init(trie_t * trie);
int trie_addWord(trie_t *trie, const char *word);
boolean trie_removeWord(trie_t *trie, const char *word);
void trie_destroy(trie_t *trie);

/** -- Trie find functions -- */

/** Query if trie has a specified word */
boolean trie_hasWord(trie_t *trie, const char *word);

/** Query if trie has a specified word */
boolean trie_hasPrefix(trie_t *trie, const char *prefix);

/** Query if trie has words starting with the specified prefix */
int trie_findPrefixesCount(trie_t *trie, const char *prefix);

/** Return words that begin with the specified prefix */
int trie_findPrefixMatches(trie_t *trie, const char *prefix, queue_t *queue);

/** -- Trie find functions -- */

#endif /** __TRIE_H__ */
