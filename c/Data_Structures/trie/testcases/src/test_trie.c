#include <trie.h>
#include <assert.h>

int
main(void)
{
	trie_t trie;

	trie_init(&trie);

	trie_addWord(&trie, "hac");
	trie_addWord(&trie, "hackerrank");
	trie_addWord(&trie, "abc");
	trie_addWord(&trie, "abc");
	trie_addWord(&trie, "abc");
	trie_addWord(&trie, "abcde");
	trie_addWord(&trie, "aaab");
	trie_addWord(&trie, "a");
	
	assert(trie_findPrefixesCount(&trie, "") == 6);
	assert(trie_len(&trie) == 6); //"abc" is repeated thrice, but is only stored once in the trie
	assert(trie_frequency(&trie, "abc") == 3);
	assert(trie_frequency(&trie, "") == 0);
	assert(trie_frequency(&trie, "a") == 1);
	assert(trie_frequency(&trie, "hac") == 1);
	assert(trie_frequency(&trie, "unknown") == 0);

	assert(trie_findPrefixesCount(&trie, "hac") == 2);
	assert(trie_findPrefixesCount(&trie, "hak")==0);
	assert(trie_findPrefixesCount(&trie, "h")==2);

	assert(trie_hasWord(&trie, "hac") == TRUE);
	assert(trie_hasWord(&trie, "hack") == FALSE);
	assert(trie_hasWord(&trie, "hak") == FALSE);

	assert(trie_hasPrefix(&trie, "hac") == TRUE);
	assert(trie_hasPrefix(&trie, "hack") == TRUE);
	assert(trie_hasPrefix(&trie, "hak") == FALSE);

	printf("All testcases pass!\n");

	return 0;
}
