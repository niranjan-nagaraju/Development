import sys
sys.path.append("../../../../")
from data_structures.trees.tries.simple_trie import Trie, Node


# trie node testcases
def node_testcases():
	n = Node()
	assert((not n) == False)
	assert(len(n) == 0)

	n.add('A')
	assert(not n == False)
	assert(len(n) == 1)

	n.eow = True
	assert(not n == False)
	assert(len(n) == 1)
	assert(n.eow == True)
	assert(n.end_of_word == True)
	assert(n.frequency == 1)
	n.children['A'] = 0x1234 # just a test address

	n.add('A') # this should do nothing
	assert(not n == False)
	assert(len(n) == 1)
	assert(n.eow == True)
	assert(n.end_of_word == True)
	assert(n.frequency == 1)
	assert(n.children['A'] == 0x1234)
	assert(n.getChildren('A') == 0x1234)

	n.add('B')
	assert(len(n) == 2)
	assert(not n.getChildren('B') == True) # empty child node
	assert(n.getChildren('B').frequency == 0) # empty child node
	assert(n.getChildren('B').eow == False) # empty child node

	n.setChildren('A') # create a new node and make it the child node of 'A' at root
	assert(n.getChildren('A') != 0x1234)
	nc = n.getChildren('A')
	nc.add('B')
	nc.eow = True # this means 'A' is a word
	assert(nc.eow == True)
	assert(nc.end_of_word == True)
	assert(nc.frequency == 1)

	assert(str(nc) == "[1]: ['B']")
	ncc = nc.remove('B')
	assert(ncc != None)
	assert(len(ncc) == 0)

	# 'A" is not set in nc
	ncc = nc.remove('A')
	assert(ncc == None)

	assert(str(n) == "[2]: ['A', 'B']")
	assert(str(nc) == "[0]: []")

	nc.add('C') # AC
	assert(str(nc) == "[1]: ['C']")
	nc.getChildren('C').eow = True
	assert(n.getChildren('A').getChildren('C').eow == True)
	assert(n.getChildren('A').getChildren('C').frequency == 1)

	# removing 'A' should not do anything at this point,
	# since there's AC on the trie
	node = n.remove('A')
	assert(node == nc)
	assert(str(node) == "[1]: ['C']")


	# try to remove 'C' from child node
	node = nc.remove('C')
	assert(len(nc) == 0) # 'C' was removed, and its empty child node too
	assert(len(node) == 0) # child node returned was empty


# trie node testcases

# trie testcases

'''
Test add word to trie (iterative version)
'''
def test_add(recursive=False):
	trie = Trie()
	assert(trie.num_words == 0)

	add_fn = trie.add
	if recursive:
		add_fn = trie.add_r

	add_fn("abcd")
	assert(trie.num_words == 1)
	assert(trie.root.getChildren('a').end_of_word == False)
	assert(trie.root.getChildren('a').frequency == 0)
	assert(trie.root.getChildren('a').getChildren('b').end_of_word == False)
	assert(trie.root.getChildren('a').getChildren('b').frequency == 0)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == False)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 0)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').frequency == 1)

	add_fn("abc", "first word")
	assert(trie.num_words == 2)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 1)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').data == "first word")
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').frequency == 1)

	add_fn("abc") # Increase frequency
	assert(trie.num_words == 2)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').data == None)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 2)


def test_hasPrefix(recursive=False):
	trie = Trie()
	assert(trie.hasPrefix("word") == False)

	trie.add("word")
	assert(trie.root.getChildren('w').getChildren('o').getChildren('r').getChildren('d').end_of_word == True)

	trie.add_r("words")
	trie.add("sword")

	assert(trie.root.getChildren('w').getChildren('o').getChildren('r').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('w').getChildren('o').getChildren('r').getChildren('d').getChildren('s').end_of_word == True)

	assert(len(trie) == 3)

	hasPrefix_fn = trie.hasPrefix
	if recursive:
		hasPrefix_fn = trie.hasPrefix_r

	assert(hasPrefix_fn("word") == True)
	assert(hasPrefix_fn("wor") == True)
	assert(hasPrefix_fn("wort") == False)
	assert(hasPrefix_fn("words") == True)
	assert(hasPrefix_fn("sword") == True)
	assert(hasPrefix_fn("swo") == True)
	assert(hasPrefix_fn("so") == False)



def test_hasWord(recursive=False):
	trie = Trie()
	assert(trie.hasWord("word") == False)

	trie.add("word")
	trie.add("words")
	trie.add("sword")
	assert(len(trie) == 3)

	hasWord_fn = trie.hasWord
	if recursive:
		hasWord_fn = trie.hasWord_r

	assert(hasWord_fn("word") == True)
	assert(hasWord_fn("wort") == False)
	assert(hasWord_fn("words") == True)
	assert(hasWord_fn("wor") == False)
	assert(hasWord_fn("sword") == True)
	assert(hasWord_fn("swords") == False)


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



def trie_testcases():
	test_add()
	test_add(recursive=True)
	test_hasPrefix()
	test_hasPrefix(recursive=True)
	test_hasWord()
	test_hasWord(recursive=True)
	test_frequency()



# trie testcases




if __name__ == '__main__':
	node_testcases()
	trie_testcases()

