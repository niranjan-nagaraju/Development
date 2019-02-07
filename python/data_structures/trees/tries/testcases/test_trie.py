import sys
sys.path.append("../../../../")
from data_structures.trees.tries.trie import Trie, TrieEmptyError



def test_add():
	trie = Trie()

	assert(len(trie) == 0)
	assert(not trie == True)
	trie.add("word")
	assert(len(trie) == 1)

	node = trie.root
	assert(not node['w'] == False)
	assert(len(node) == 1) # only 1 character 'w' set in root
	assert(repr(node) == "[1]: ('w', $:False f:0 pc:1)")

	node = node['w'].children
	assert(not node == False)
	assert(not node['o'] == False)
	assert(len(node) == 1) # only 1 character 'o' set in level-1
	assert(repr(node) == "[1]: ('o', $:False f:0 pc:1)")

	node = node['o'].children
	assert(not node == False)
	assert(not node['r'] == False)
	assert(len(node) == 1) # only 1 character 'r' set in level-2
	assert(repr(node) == "[1]: ('r', $:False f:0 pc:1)")

	node = node['r'].children
	assert(not node == False)
	assert(not node['d'] == False)
	assert(len(node) == 1) # only 1 character 'd' set in level-3
	assert(repr(node) == "[1]: ('d', $:True f:1 pc:1)")

	# Add a second word, "words"
	trie.add("words")
	assert(len(trie) == 2)

	node = trie.root
	assert(not node['w'] == False)
	assert(len(node) == 1) # only 1 character 'w' set in root
	assert(repr(node) == "[1]: ('w', $:False f:0 pc:2)")

	node = node['w'].children
	assert(not node == False)
	assert(not node['o'] == False)
	assert(len(node) == 1) # only 1 character 'o' set in level-1
	assert(repr(node) == "[1]: ('o', $:False f:0 pc:2)")

	node = node['o'].children
	assert(not node == False)
	assert(not node['r'] == False)
	assert(len(node) == 1) # only 1 character 'r' set in level-2
	assert(repr(node) == "[1]: ('r', $:False f:0 pc:2)")

	node = node['r'].children
	assert(not node == False)
	assert(not node['d'] == False)
	assert(len(node) == 1) # only 1 character 'd' set in level-3
	assert(repr(node) == "[1]: ('d', $:True f:1 pc:2)")

	node = node['d'].children
	assert(not node == False)
	assert(not node['s'] == False)
	assert(len(node) == 1) # only 1 character 's' set in level-4
	assert(repr(node) == "[1]: ('s', $:True f:1 pc:1)")

	# Add a third word, "abc"
	trie.add("abc")
	assert(len(trie) == 3)

	node = trie.root
	assert(not node['a'] == False)
	assert(len(node) == 2) # only 2 characters 'w' and 'a' set in root
	assert(repr(node) == "[2]: ('a', $:False f:0 pc:1) ('w', $:False f:0 pc:2)")

	node = node['a'].children
	assert(not node == False)
	assert(not node['b'] == False)
	assert(len(node) == 1) # only 1 character 'b' set in level-1 below 'a'
	assert(repr(node) == "[1]: ('b', $:False f:0 pc:1)")

	node = node['b'].children
	assert(not node == False)
	assert(not node['c'] == False)
	assert(len(node) == 1) # only 1 character 'c' set in level-2 below 'b'
	assert(repr(node) == "[1]: ('c', $:True f:1 pc:1)")




def test_hasWord():
	trie = Trie()
	try:
		assert(trie.hasWord("word") == False)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'hasWord(): Trie is empty'")

	trie.add("word")
	trie.add("words")
	trie.add("sword")

	assert(len(trie) == 3)
	assert(trie.hasWord("word") == True)
	assert(trie.hasWord("wort") == False)
	assert(trie.hasWord("words") == True)
	assert(trie.hasWord("wor") == False)
	assert(trie.hasWord("sword") == True)
	assert(trie.hasWord("swords") == False)


def test_hasPrefix():
	trie = Trie()
	try:
		assert(trie.hasPrefix("word") == False)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'hasPrefix(): Trie is empty'")

	trie.add("word")
	trie.add("words")
	trie.add("sword")

	assert(len(trie) == 3)
	assert(trie.hasPrefix("word") == True)
	assert(trie.hasPrefix("wor") == True)
	assert(trie.hasPrefix("wort") == False)
	assert(trie.hasPrefix("words") == True)
	assert(trie.hasPrefix("sword") == True)
	assert(trie.hasPrefix("swo") == True)
	assert(trie.hasPrefix("so") == False)



def test_frequency():
	trie = Trie()
	try:
		assert(trie.frequency("word") == 0)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'frequency(): Trie is empty'")

	trie.add("word")
	trie.add("word")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("sword")

	assert(len(trie) == 3)
	assert(trie.frequency("word") == 2)
	assert(trie.frequency("wor") == 0)
	assert(trie.frequency("wort") == 0)
	assert(trie.frequency("words") == 4)
	assert(trie.frequency("sword") == 1)
	assert(trie.frequency("swo") == 0)
	assert(trie.frequency("so") == 0)


def test_countPrefix():
	trie = Trie()
	try:
		assert(trie.countPrefix("word") == 0)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'countPrefix(): Trie is empty'")

	trie.add("word")
	trie.add("word")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("sword")
	trie.add("ward")

	assert(len(trie) == 4)
	assert(trie.countPrefix("w") == 3)
	assert(trie.countPrefix("word") == 2)
	assert(trie.countPrefix("wor") == 2)
	assert(trie.countPrefix("wort") == 0)
	assert(trie.countPrefix("words") == 1)
	assert(trie.countPrefix("sword") == 1)
	assert(trie.countPrefix("swo") == 1)
	assert(trie.countPrefix("so") == 0)



if __name__ == '__main__':
	test_add()
	test_hasWord()
	test_hasPrefix()
	test_frequency()
	test_countPrefix()
	

