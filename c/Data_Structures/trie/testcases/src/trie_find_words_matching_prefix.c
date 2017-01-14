#include <trie.h>
#include <assert.h>

int
main(void)
{
	trie_t trie;

	trie_init(&trie);

	trie_addWord(&trie, "hack");
	trie_addWord(&trie, "hacs");
	trie_addWord(&trie, "hackerrank");

	trie_findPrefixMatches(&trie, "hac", 0);
	return 0;
}
