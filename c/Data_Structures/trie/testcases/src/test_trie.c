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
	assert(trie_findPrefixesCount(&trie, "h")==2);


	assert(trie_hasWord(&trie, "hac") == TRUE);
	assert(trie_hasWord(&trie, "hack") == FALSE);
	assert(trie_hasWord(&trie, "hak") == FALSE);

	assert(trie_hasPrefix(&trie, "hac") == TRUE);
	assert(trie_hasPrefix(&trie, "hack") == TRUE);
	assert(trie_hasPrefix(&trie, "hak") == FALSE);

	return 0;
}
