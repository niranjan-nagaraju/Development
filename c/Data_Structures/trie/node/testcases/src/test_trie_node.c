#include <trie_node.h>
#include <assert.h>
#include <stdio.h>

int
main(void)
{
	trie_node_t *tnode, *tnode2;

	tnode = trie_node_create();
	trie_node_set(tnode, 'a', TRUE, 1);
	trie_node_item_t *item = tnode->items['a'];
	item->prefix_count = 1;
	
	printf("Node key: %c\n", item->key);
	printf("Node prefix count: %d\n", item->prefix_count);
	printf("eow: %d, frequency: %d\n", item->isEndOfWord, item->frequency);

	tnode2 = trie_node_create();
	item = tnode2->items['b']; //representing 'ab'
	trie_node_set(tnode2, 'b', TRUE, 100);
	item->prefix_count = 2;

	printf("Node key: %c\n", item->key);
	printf("Node prefix count: %d\n", item->prefix_count);
	printf("eow: %d, frequency: %d\n", item->isEndOfWord, item->frequency);

	trie_node_set(tnode2, 'c', TRUE, 10);
	item = tnode2->items['c']; //representing 'ac'
	item->prefix_count = 3;

	printf("Node key: %c\n", item->key);
	printf("Node prefix count: %d\n", item->prefix_count);
	printf("eow: %d, frequency: %d\n", item->isEndOfWord, item->frequency);

	return 0;
}
