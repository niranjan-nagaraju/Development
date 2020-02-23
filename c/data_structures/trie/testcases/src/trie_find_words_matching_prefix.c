#include <trie.h>
#include <assert.h>

/** 
 * Print callback for every element in a queue
 * of suffix-strings, prepend prefix-string to them
 * so the word is complete
 */ 
void
printQueueElemsAndPrefix1(void *object)
{
	printf("%s ", (char*)object);
}


int
main(void)
{
	trie_t trie;
	queue_t q1, q2, q3;

	trie_init(&trie);

	trie_addWord(&trie, "hack");
	trie_addWord(&trie, "hacs");
	trie_addWord(&trie, "hackerrank");
	trie_addWord(&trie, "hakerrank");
	trie_addWord(&trie, "abc");

	queue_init(&q1);
	queue_init(&q2);
	queue_init(&q3);

	assert(trie_findPrefixMatches(&trie, "hac", &q1) == 3);
	assert(trie_findPrefixesCount(&trie, "hac") == 3);
	assert(trie_findPrefixMatches(&trie, "", &q2) == 5); /** All words in the trie */
	assert(trie_findPrefixesCount(&trie, "") == 5);
	assert(trie_findPrefixMatches(&trie, 0, &q3) == 5);  /** All words in the trie */
	assert(trie_findPrefixesCount(&trie, 0) == 5);

	printf("Prefix 'hac' matches:\n");
	queue_print(&q1, printQueueElemsAndPrefix1);

	printf("Prefix '' matches:\n");
	queue_print(&q2, printAsString);

	printf("Prefix (null) matches:\n");
	queue_print(&q3, printAsString);

	return 0;
}



/** 
  Sample run:

  Prefix 'hac' matches:
  [3]: hack  hackerrank  hacs  
  Prefix '' matches:
  [5]: abc  hack  hackerrank  hacs  hakerrank  
  Prefix (null) matches:
  [5]: abc  hack  hackerrank  hacs  hakerrank 
*/
