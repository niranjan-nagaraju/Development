import sys
sys.path.append("../../../../")
from data_structures.trees.tries.simple_trie import Trie


def test_add():
	trie = Trie()
	trie.add("abcd")

	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').frequency == 1)



if __name__ == '__main__':
	test_add()
