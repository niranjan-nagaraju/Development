#include <trie.h>
#include <assert.h>

int
main(void)
{
	trie_t trie;

	trie_init(&trie);

	trie_addWord(&trie, "hac");
	trie_addWord(&trie, "hackerrank");

	assert(trie_findPrefixesCount(&trie, "hac") == 2);
	assert(trie_findPrefixesCount(&trie, "hak")==0);

	return 0;
}
