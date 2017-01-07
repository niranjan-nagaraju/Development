#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>

/** 
 * Solve contacts - prefix count using a trie (full solution in one file)
 * https://www.hackerrank.com/challenges/contacts
 */

typedef enum 
{
    FALSE,
    TRUE
} boolean;


#define MAX_CHARS_IN_UNIVERSE 26


typedef struct trie_node_s {
    struct trie_node_s *children[MAX_CHARS_IN_UNIVERSE];

    /** Is there a word that ends in char at current node */
    boolean isEndOfWord;

    /** 
     * Running count of the number of words 
     * that begin with the chars at current node 
     */
    int prefix_count;
} trie_node_t;


typedef struct trie_s {
    trie_node_t *root;

    /** Number of entries in the trie */
    int numWords;   
}trie_t;

/** Create a new node */
trie_node_t *createNode(void);

int trie_addWord(trie_t *trie, const char *word);
int trie_findPrefixesCount(trie_t *trie, const char *prefix);

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

    return 0;
}


int
trie_findPrefixesCount(trie_t *trie, const char *prefix)
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
    return (trav ? trav->prefix_count : 0);
}

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

