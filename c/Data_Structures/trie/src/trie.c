#include <trie.h>
#include <trie_internal.h>
#include <trie_node.h>
#include <common.h>

#include <string.h>
#include <errno.h>


void
trie_init(trie_t * trie)
{
	trie->root = trie_node_create();
	trie->numWords = 0;
}

int
trie_addWord(trie_t *trie, const char *word)
{
	int i;
	trie_node_t *trav;

	if (!trie || !trie->root)
		return -EINVAL;

	trav = trie->root;
	for(i=0; word[i]; i++) {
		int c = charToInt(word[i]);

		if (!trav->children[c]) {
			trav->children[c] = trie_node_create();
		}
		trav->children[c]->prefix_count++;

		trav = trav->children[c];
	}

	trav->isEndOfWord = TRUE;

	trie->numWords ++;

	return 0;
}


/** Return node following the 'prefix' if found, else null */
static trie_node_t *
trie_findPrefixNode(trie_t *trie, const char *prefix)
{
	int i, n;
	trie_node_t *trav;

	if (!trie || !trie->root)
		return 0;

	/** 
	 * Technically, a null-prefix matches everything
	 * in the trie ;)
	 */
	if (!prefix || prefix[0] == 0)
		return trie->root;


	trav = trie->root;
	for(i=0; prefix[i]; i++) {
		int c = charToInt(prefix[i]);

		if (!trav->children[c]) 
			return 0;

		trav = trav->children[c];
	}

	return trav;
}

boolean
trie_hasWord(trie_t *trie, const char *word)
{
	trie_node_t *trav;

	trav = trie_findPrefixNode(trie, word);
	return (trav && trav->isEndOfWord);
}



int
trie_findPrefixesCount(trie_t *trie, const char *prefix)
{
	trie_node_t *trav = trie_findPrefixNode(trie, prefix);

	return trav ? trav->prefix_count : 0;
}


boolean
trie_hasPrefix(trie_t *trie, const char *prefix)
{
	trie_node_t *trav;

	trav = trie_findPrefixNode(trie, prefix);

	return (trav != NULL);
}



/** 
 * DFS search from the matching prefix node 
 * Return a queue of suffixes and the number of matching words.
 */ 
static void
dfs_search(trie_node_t *node, queue_t *suffix_queue, char *suffix, int suffixlen)
{
	int i;
	char *s;

	if (!node)
		return;

	/** We have found a full-word, 
	 * Pull a copy of it and store it as a result 
	 */
	if (node->isEndOfWord) {
		s = strndup(suffix, suffixlen);
		/** 
		 * Copy only 'suffixlen' chars and close the string for good measure.
		 * Some of the higher depth chars might be attached to it
		 *
		 * 'suffixlen' tells us what depth we are at and therefore our
		 * word's suffix-part cannot be bigger than that
		 *
		 * 'strndup' does this for us anyways, but we can't be too sure.
		 */
		s[suffixlen] = 0; 

		enqueue(suffix_queue, (void *)s);
	}

	for (i=0; i< MAX_CHARS_IN_UNIVERSE; i++) {
		int first = 0;
		if (node->children[i]) {
			suffix[suffixlen] = i + 'a';

			dfs_search(node->children[i], suffix_queue, suffix, suffixlen+1);
		}
	}
	
	return;
}

int
trie_findPrefixMatches(trie_t *trie, const char *prefix, queue_t *suffix_queue)
{
	#define MAX_WORDLEN 100
	char *suffix = malloc(MAX_WORDLEN);

	trie_node_t *node = trie_findPrefixNode(trie, prefix);

	/** prefix does not exist */
	if (!node) {
		return 0;
	}

	memset(suffix, 0, MAX_WORDLEN);

	dfs_search(node, suffix_queue, suffix, 0);

	return queue_length(suffix_queue);
}
