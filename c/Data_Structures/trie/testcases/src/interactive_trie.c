#include <trie.h>
#include <stdio.h>
#include <assert.h>

/** 
 * TODO: Move these to essential test_common
 * so other test routines can also use it
 */ 
void
show_prompt(char *prompt)
{
	printf("%s> ", prompt);
}

int
getInputLine(char *cmd, char *arg, int maxlen)
{
	int i=0, idx=0;
	int c;
	char *s = cmd; /** start reading into cmd */

	for (i=0; i<maxlen; i++) {
		c = getchar();

		if (c == EOF)
			return EOF;

		if (c == ' ') {
			s[idx] = 0;
			s = arg;
			idx = 0;

			continue; /** skip spaces */
		}

		if (c == '\n')
			break;

		s[idx++] = c;
	}

	s[idx] = 0;

	return 0;
}

void
print_queue_matches(queue_t *q, const char *prefix)
{
	char *s;
	while (s = dequeue(q)) {
		printf("%s%s\n", prefix, s);
	}
}

int
process_cmds(trie_t *trie, char *cmd, char *arg)
{
	queue_t q;
	queue_init(&q);

	if (!cmd || !cmd[0]) {
		return 0;
	}

	if (!strncmp(cmd, "help", 4)) {
		printf("Commands: \n"
				"add <word> :: Insert <word> into trie\n"
				"remove <word> :: Remove <word> from trie\n"
				"find <prefix> :: Find words that match <prefix>\n"
				"hasWord <word> :: Does trie have <word>\n"
				"hasPrefix <prefix> :: Does trie have prefix\n"
				"show :: Show all words in trie\n"
				"help :: Show this message\n"
				);
	} else if(!strncmp(cmd, "add", 3)) {
		if (!arg || !arg[0]) {
			printf("Invalid Input\n");
			goto done;
		}
		if (trie_addWord(trie, arg) == 0) {
			printf("Added '%s' to trie.\n", arg);
		} else {
			printf("Error adding '%s' to trie.\n", arg);
		}
	} else if (!strncmp(cmd, "find", 4)) {
		printf("Matches: %d\n", trie_findPrefixMatches(trie, arg, &q));
		print_queue_matches(&q, arg);
	} else if (!strncmp(cmd, "show", 4)) {
		printf("Words in Trie: \n", trie_findPrefixMatches(trie, 0, &q));
		print_queue_matches(&q, "");
	} else if  (!strncmp(cmd, "remove", 7)) {
		if (!arg || !arg[0]) {
			printf("Invalid Input\n");
			goto done;
		}
		//printf("Remove '%s' from trie\n", arg);
		printf("Not supported as of now.\n");
	} else if (!strncmp(cmd, "hasWord", 7)) {
		if (!arg || !arg[0]) {
			printf("Invalid Input\n");
			goto done;
		}
		printf("Word '%s' %s in trie\n", arg, trie_hasWord(trie, arg) ? "exists" : "does not exist");
	} else if (!strncmp(cmd, "hasPrefix", 7)) {
		if (!arg || !arg[0]) {
			printf("Invalid Input\n");
			goto done;
		}
		printf("Prefix '%s' matches %d words in trie\n", arg, trie_findPrefixesCount(trie, arg));
	} else if (!strncmp(cmd, "quit", 7)) {
		printf("Bye.\n");
		return 1;
	} else {
		printf("Unknown command\n");
	}
	
done:
	/** Done processing, Clear input */
	cmd[0] = 0;
	arg[0] = 0;

	//queue_destroy(&q);

	return 0;
}

void
interactive_shell(void)
{
#define MAX_LINE_LEN 100
#define MAX_CMD_LEN 10
	trie_t trieObj;
	trie_t *trie = &trieObj;
	char cmd[MAX_CMD_LEN] = {0}, arg[MAX_LINE_LEN] = {0};
	int ret=0;

	trie_init(trie);

	while (1) {
		show_prompt("*Trie");
		if (getInputLine(cmd, arg, MAX_LINE_LEN) == EOF)
			break;

		if (process_cmds(trie, cmd, arg)) {
			/** Done processing at 'quit' command */
			break;
		}
	}
}

int
main(void)
{
	interactive_shell();

	return 0;
}

/** 
Sample session
[21:14:05 trie]$ ./testcases/out/linux/interactive_trie
*Trie> sho
Unknown command
*Trie> show
Words in Trie: 
*Trie> add word
Added 'word' to trie.
*Trie> show
Words in Trie: 
word
*Trie> add hack
Added 'hack' to trie.
*Trie> show
Words in Trie: 
hack
word
*Trie> add abc
Added 'abc' to trie.
*Trie> add abd
Added 'abd' to trie.
*Trie> find ab
Matches: 2
abc
abd
*Trie> find a
Matches: 2
abc
abd
*Trie> add ace
Added 'ace' to trie.
*Trie> find a
Matches: 3
abc
abd
ace
*Trie> find ab
Matches: 2
abc
abd
*Trie> hasPrefix ab
Prefix 'ab' matches 2 words in trie
*Trie> hasPrefix ac
Prefix 'ac' matches 1 words in trie
*Trie> hasPrefix ad
Prefix 'ad' matches 0 words in trie
*Trie> hasWord ab
Word 'ab' does not exist in trie
*Trie> hasWord abc
Word 'abc' exists in trie
*Trie> hasWord abcd
Word 'abcd' does not exist in trie
*Trie> quit
Bye.

*/
