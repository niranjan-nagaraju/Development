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

char *
duplicate_string(char *suffix)
{
	char *dup = malloc(100);
	char *tmp = dup;

	memset(dup, 0, 100);

	while (*tmp++ = *suffix++);

	return dup;
}


static void
dfs_search(trie_node_t *node, queue_t *queue, const char *prefix, char *suffix, int suffixlen)
{
	int i;
	char *s;

	if (!node)
		return;

	printf("End of word: %d\n", node->isEndOfWord);
	if (node->isEndOfWord) {
		s = duplicate_string(suffix);
		printf("Found word: %s%s, suffixlen: %d, Numprefixes: %d\n", prefix, s, suffixlen, node->prefix_count);

		//if(node->prefix_count == 1) 
			suffixlen--;
	} else {
		s = suffix;
	}
	for (i=0; i< MAX_CHARS_IN_UNIVERSE; i++) {
		int first = 0;
		if (node->children[i]) {
			suffix[suffixlen] = i + 'a';

			printf("Char: %c\n", i+'a');

			dfs_search(node->children[i], queue, prefix, s, suffixlen+1);
		}
	}
	
	return;
}

int
trie_findPrefixMatches(trie_t *trie, const char *prefix, queue_t *queue)
{
	char *suffix = malloc(100);
	
	trie_node_t *node = trie_findPrefixNode(trie, prefix);

	/** prefix does not exist */
	if (!node) {
		return 0;
	}

	printNL();

	memset(suffix, 0, 100);

	dfs_search(node, queue, prefix, suffix, 0);
	return 0;
}
