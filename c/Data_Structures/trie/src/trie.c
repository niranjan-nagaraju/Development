#include <trie.h>
#include <trie_internal.h>
#include <trie_node.h>
#include <common.h>

#include <string.h>
#include <errno.h>


/** Return node following the 'prefix' if found, else null */
static trie_node_t *
trie_findPrefixNode(trie_t *trie, const char *prefix)
{
#if 0
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
#endif
	return 0;
}


static void
trie_updateFrequency(trie_t *trie, const char *word)
{
	trie_node_t *node = trie_findPrefixNode(trie, word);
	node->items[word[strlen(word)-1]]->frequency += 1; 
}


/** 
 * DFS search from the matching prefix node 
 * Return a queue of suffixes and the number of matching words.
 */ 
static void
dfs_search(trie_node_t *node, queue_t *suffix_queue, char *suffix, int suffixlen)
{
#if 0
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
#endif
	
	return;
}




void
trie_init(trie_t * trie)
{
	trie->root = 0;
	trie->numWords = 0;
}


int
trie_addWord(trie_t *trie, const char *word)
{
	int i, err;
	trie_node_t *trav;

	if (!trie)
		return -EINVAL;

	if (!trie->root)
		trie->root = trie_node_create();

	/** 
	 * If the word already exists in the trie, just 
	 * update frequency and return
	 */
	if(trie_hasWord(trie, word)) {
		trie_updateFrequency(trie, word);
		return 0;
	}

	trie_node_add(trie->root, word[0]);
	trav = trie->root;
	for(i=1; word[i]; i++) {
		trie_node_t *child;
		/**
		 * there's no child corresponding to word[i] from word[i-1]
		 * create a new node, and assign it as a child-node of word[i-1]
		 */
		if ( !(child = trie_node_getChildNode(trav, word[i-1]))) {
			child = trie_node_create();

			/** memory allocation failed */
			if (!child)
				return -ENOMEM;

			trie_node_setChildNode(trav, word[i-1], child);
		}

		if ( (err = trie_node_add(child, word[i])) < 0 )
			return err;

		trav = child;
	}

	trie_node_setEndOfWord(trav, word[i-1], TRUE);

	/** First time, this 'word' as a whole was seen */
	if (trie_node_getFrequency(trav, word[i-1]) == 1)
		trie->numWords ++;

	return 0;
}


boolean
trie_hasWord(trie_t *trie, const char *word)
{
#if 0
	trie_node_t *trav;

	trav = trie_findPrefixNode(trie, word);
	return (trav && trav->isEndOfWord);
#endif
	return 0;
}



int
trie_findPrefixesCount(trie_t *trie, const char *prefix)
{
#if 0
	trie_node_t *trav = trie_findPrefixNode(trie, prefix);

	return trav ? trav->prefix_count : 0;
#endif
	return 0;
}


boolean
trie_hasPrefix(trie_t *trie, const char *prefix)
{
#if 0
	trie_node_t *trav;

	trav = trie_findPrefixNode(trie, prefix);

	return (trav != NULL);
#endif
	return 0;
}



int
trie_findPrefixMatches(trie_t *trie, const char *prefix, queue_t *suffix_queue)
{
#if 0
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
#endif
	return 0;
}
