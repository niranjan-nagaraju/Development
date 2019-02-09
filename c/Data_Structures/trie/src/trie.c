#include <trie.h>
#include <trie_internal.h>
#include <trie_node.h>
#include <common.h>

#include <string.h>
#include <errno.h>

/******************************************
 * Internal helper functions for the trie *
 ******************************************/

/** Return node containing the 'prefix' if found, else null */
static trie_node_t *
trie_findPrefixNode(trie_t *trie, const char *prefix)
{
	int i;
	trie_node_t *trav, *last;

	if (!trie || !trie->root)
		return 0;

	/** 
	 * Technically, a null-prefix/ "" matches everything
	 * in the trie ;)
	 */
	if (!prefix || prefix[0] == 0)
		return trie->root;


	trav = trie->root;
	last = trav;
	for(i=0; prefix[i]; i++) {
		if (!trav || !trav->items[prefix[i]]) 
			return 0;

		last = trav;
		trav = trie_node_getChildNode(trav, prefix[i]);
	}

	/** 
	 * trav is one level below "prefix", and therefore one level too far.
	 * last is at the end node containing prefix.
	 */

	return last;
}


/**
 * Return TRUE if the word already exists in the trie (after updating its frequency)
 * else, return FALSE
 */
static boolean
trie_update_frequency_if_word_exists(trie_t *trie, const char *word)
{
	trie_node_t *node = trie_findPrefixNode(trie, word);
	int n;

	/** The word doesn't exist in the trie */
	if (!node)
		return FALSE;


	n = strlen(word);
	/** 
	 * 'word' actually exists in the trie, but is not a whole word
	 * it's actually a prefix for one or more words added to the trie
	 */ 
	if (!trie_node_getEndOfWord(node, word[n-1]))
		return FALSE;

	node->items[word[n-1]]->frequency += 1; 
	return TRUE;
}


/** 
 * DFS search from the matching prefix node 
 * Return a queue of suffixes and the number of matching words.
 */ 
static void
dfs_search(trie_node_t *node, queue_t *words_queue, char *word, int wordlen)
{
	int i;
	char *s;

	if (!node)
		return;

	/** ASCII readable characters are 0-127 */
	for (i=0; i<MAX_CHARS_IN_UNIVERSE; i++) {
		trie_node_item_t *item = node->items[i];

		/** character 'i' doesn't exist in the current trie node */
		if (!item)
			continue;

		/** append current character to word */
		word[wordlen] = i;
		word[wordlen+1] = 0;

		/** We have found a full-word, 
		 *  Pull a copy of it and store it as a result 
		 */
		if (item->isEndOfWord) {
			s = strndup(word, wordlen+1);
			/** 
			 * Copy only 'wordlen+1' chars and close the string for good measure.
			 * Some of the higher depth chars might be attached to it
			 *
			 * 'strndup' does this for us anyways, but we can't be too sure.
			 */
			s[wordlen+1] = 0; 

			enqueue(words_queue, (void *)s);
		}

		dfs_search(trie_node_getChildNode(node, i), words_queue,  word, wordlen+1);
	}
}


/******************************************
 * Internal helper functions for the trie *
 ******************************************/


/*************************************
 * Public API functions for the trie *
 *************************************/

/**
 * Initialize trie
 */ 
void
trie_init(trie_t * trie)
{
	trie->root = 0;
	trie->numWords = 0;
}

/**
 * Number of words in the trie
 */
int trie_len(trie_t *trie)
{
	if (!trie)
		return 0;

	return trie->numWords;
}


/**
 * Add a word into the trie
 * if the word already exists, just update frequency and return
 */
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
	if (trie_update_frequency_if_word_exists(trie, word))
		return 0;

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
	trie->numWords ++;

	return 0;
}

/**
 * Return number of times 'word' was added to the trie
 */
int
trie_frequency(trie_t *trie, const char *word)
{
	trie_node_t *node;
	int n;

	/** word being NULL or "" is not really a word, 
	 * and won't be looked up in the trie
	 */
	if (!trie || !trie->root || !word || !word[0])
		return 0;

	node = trie_findPrefixNode(trie, word);
	if(!node)
		return 0;

	n = strlen(word);
	/** 
	 * 'word' actually exists in the trie, but is not a whole word
	 * it's actually a prefix for one or more words added to the trie
	 */ 
	if (!trie_node_getEndOfWord(node, word[n-1]))
		return 0;

	return trie_node_getFrequency(node, word[n-1]);
}



/**
 * Return true if 'word' exists in the trie
 * false otherwise
 */
boolean
trie_hasWord(trie_t *trie, const char *word)
{
	trie_node_t *node;
	int n;

	/** word being NULL or "" is not really a word, 
	 * and won't be looked up in the trie
	 */
	if (!trie || !trie->root || !word || !word[0])
		return FALSE;

	node = trie_findPrefixNode(trie, word);
	if(!node)
		return FALSE;

	n = strlen(word);
	/** 
	 * 'word' actually exists in the trie, but is not a whole word
	 * it's actually a prefix for one or more words added to the trie
	 */ 
	if (!trie_node_getEndOfWord(node, word[n-1]))
		return FALSE;

	return TRUE;
}


/**
 * Return number of words with the prefix
 * if the prefix is NULL or "", return the total number of words in the trie
 */
int
trie_findPrefixesCount(trie_t *trie, const char *prefix)
{
	trie_node_t *node;
	int n;

	if(!trie || !trie->root)
		return 0;

	if(!prefix || !prefix[0])
		return trie->numWords;

	node = trie_findPrefixNode(trie, prefix);
	if(!node)
		return 0;

	n = strlen(prefix);
	return trie_node_getPrefix_count(node, prefix[n-1]);
}


/**
 * Return true if there are words with the specified prefix in the trie
 * false otherwise
 */
boolean
trie_hasPrefix(trie_t *trie, const char *prefix)
{
	trie_node_t *node;

	if(!trie || !trie->root)
		return FALSE;

	node = trie_findPrefixNode(trie, prefix);

	return (node != NULL);
}



int
trie_findPrefixMatches(trie_t *trie, const char *prefix, queue_t *words_queue)
{
	#define MAX_WORDLEN 100
	char *matching_word = malloc(MAX_WORDLEN);
	int n;
	trie_node_t *node;

	if(!trie || !trie->root)
		return 0;
	
	memset(matching_word, 0, MAX_WORDLEN);

	/** empty prefix - match every word in the trie */
	if (!prefix || !prefix[0]) {
		/** start a dfs search for everything from root */
		dfs_search(trie->root, words_queue, matching_word, 0);
	} else {
		node = trie_findPrefixNode(trie, prefix);

		/** prefix does not exist */
		if (!node) {
			return 0;
		}

		n = strlen(prefix);
		/** 
		 * 'prefix' is an actual word in the trie
		 * enqueue it as one of the results
		 */ 
		if (trie_node_getEndOfWord(node, prefix[n-1]))
			enqueue(words_queue, (void *)prefix);

		/** copy prefix to a template, 
		 * all matching word-suffixes in the trie will be appended to this prefix */
		strncpy(matching_word, prefix, n);
		matching_word[n] = 0;

		/** start a dfs-search for all child-nodes in the trie starting from the child node following the prefix */
		dfs_search(trie_node_getChildNode(node, prefix[n-1]), words_queue, matching_word, n);
	}

	return queue_length(words_queue);
}
