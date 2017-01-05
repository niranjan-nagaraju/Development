#include <trie.h>
#include <string.h>
#include <stdio.h>

/** 
 * Solve contacts - prefix count using a trie 
 * https://www.hackerrank.com/challenges/contacts
 */ 

#define MAX_INPUT_SIZE 30

void
processInput(trie_t *contacts_trie)
{
	int n;
	char word[MAX_INPUT_SIZE] = {0};
	char cmds[5] = {0};

	scanf("%d", &n);

	while (n--) {
		scanf("%s %s", cmds, word);
		if(!strncmp(cmds, "add", 3)) {
			trie_addWord(contacts_trie, word);
		} else if (!strncmp(cmds, "find", 4)) {
			printf("%d\n", trie_findPrefixesCount(contacts_trie, word));
		}
	}
}

int
main(void)
{
	trie_t contacts_trie;

	trie_init(&contacts_trie);

	processInput(&contacts_trie);

	return 0;
}

