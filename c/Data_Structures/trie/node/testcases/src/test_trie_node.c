#include <trie_node.h>
#include <assert.h>
#include <stdio.h>

int
main(void)
{
	trie_node_t *tnode, *tnode2;
	trie_node_item_t *item;

	tnode = trie_node_create();
	trie_node_add(tnode, 'a');
	trie_node_setPrefix_count(tnode, 'a', 2);
	trie_node_setEndOfWord(tnode, 'a', FALSE);
	trie_node_setFrequency(tnode, 'a', 0);

	item = tnode->items['a'];
	assert(item != 0);
	assert(tnode->num_items == 1);
	assert(item->prefix_count == 2);
	assert(item->isEndOfWord == FALSE);
	assert(item->frequency == 0);

	tnode2 = trie_node_create();
	trie_node_add(tnode2, 'b');
	trie_node_setPrefix_count(tnode2, 'b', 1);
	trie_node_setEndOfWord(tnode2, 'b', TRUE);
	trie_node_setFrequency(tnode2, 'b', 10);

	item = tnode2->items['b']; //representing 'ab'
	assert(item != 0);
	assert(tnode2->num_items == 1);
	assert(item->prefix_count == 1);
	assert(item->isEndOfWord == TRUE);
	assert(item->frequency == 10);

	trie_node_add(tnode2, 'c');
	trie_node_setPrefix_count(tnode2, 'c', 2);
	trie_node_setEndOfWord(tnode2, 'c', TRUE);
	trie_node_setFrequency(tnode2, 'c', 100);

	item = tnode2->items['c']; //representing 'ac'
	assert(item != 0);
	assert(tnode2->num_items == 2);
	assert(item->prefix_count == 2);
	assert(item->isEndOfWord == TRUE);
	assert(item->frequency == 100);


	/** remove ac */
	trie_node_remove(tnode2, 'c');
	item = tnode2->items['c']; //representing 'ac'
	assert(item == 0);
	assert(tnode2->num_items == 1);

	return 0;
}
