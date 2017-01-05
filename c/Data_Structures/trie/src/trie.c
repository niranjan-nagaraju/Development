#include <trie.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

int
charToInt(char c)
{
	return tolower(c) - 'a';
}


trie_node_t *
createNode(void)
{
	trie_node_t *tmp = malloc(sizeof(trie_node_t));

	if (!tmp) 
		return 0;

	memset(tmp, 0, sizeof(trie_node_t));

	return tmp;
}


void
trie_init(trie_t * trie)
{
	trie->root = createNode();
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
			trav->children[c] = createNode();
		}
		trav->children[c]->prefix_count++;

		trav = trav->children[c];
	}

	trav->isEndOfWord = TRUE;

	trie->numWords ++;

	return 0;
}


boolean
trie_findWord(trie_t *trie, const char *word)
{
	int i;
	trie_node_t *trav;

	if (!trie || !trie->root)
		return FALSE;

	trav = trie->root;
	for(i=0; word[i]; i++) {
		int c = charToInt(word[i]);

		if (!trav->children[c]) 
			return FALSE;

		trav = trav->children[c];
	}

	return (trav && trav->isEndOfWord);
}


int
trie_findPrefixesCount(trie_t *trie, const char *prefix)
{
	int i, n;
	trie_node_t *trav;

	if (!trie || !trie->root)
		return 0;

	n = strlen(prefix);
	trav = trie->root;
	for(i=0; i<n; i++) {
		int c = charToInt(prefix[i]);

		if (!trav->children[c]) 
			return 0;

		trav = trav->children[c];
	}

	return (trav ? trav->prefix_count : 0);
}

