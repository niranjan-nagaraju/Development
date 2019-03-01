import sys
sys.path.append("../../../../")
from data_structures.trees.tries.simple_trie import Trie, Node


# trie node testcases
def node_testcases():
	n = Node()
	assert(not n == True)
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
def test_add():
	trie = Trie()
	assert(trie.num_words == 0)

	trie.add("abcd")
	assert(trie.num_words == 1)
	assert(trie.root.getChildren('a').end_of_word == False)
	assert(trie.root.getChildren('a').frequency == 0)
	assert(trie.root.getChildren('a').getChildren('b').end_of_word == False)
	assert(trie.root.getChildren('a').getChildren('b').frequency == 0)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == False)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 0)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').frequency == 1)

	trie.add("abc", "first word")
	assert(trie.num_words == 2)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 1)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').data == "first word")
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').frequency == 1)

	trie.add("abc") # Increase frequency
	assert(trie.num_words == 2)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').data == None)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 2)



'''
Test add word to trie (recursive version)
'''
def test_add_r():
	trie = Trie()
	assert(trie.num_words == 0)

	trie.add_r("abcd")
	assert(trie.num_words == 1)
	assert(trie.root.getChildren('a').end_of_word == False)
	assert(trie.root.getChildren('a').frequency == 0)
	assert(trie.root.getChildren('a').getChildren('b').end_of_word == False)
	assert(trie.root.getChildren('a').getChildren('b').frequency == 0)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == False)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 0)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').frequency == 1)

	trie.add_r("abc", "first word")
	assert(trie.num_words == 2)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 1)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').data == "first word")
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').frequency == 1)

	trie.add_r("abc") # Increase frequency
	assert(trie.num_words == 2)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').data == None)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 2)


def test_hasPrefix():
	trie = Trie()
	assert(trie.hasPrefix("word") == False)

	trie.add("word")
	assert(trie.root.getChildren('w').getChildren('o').getChildren('r').getChildren('d').end_of_word == True)

	trie.add("words")
	trie.add("sword")

	assert(trie.root.getChildren('w').getChildren('o').getChildren('r').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('w').getChildren('o').getChildren('r').getChildren('d').getChildren('s').end_of_word == True)

	assert(len(trie) == 3)
	assert(trie.hasPrefix("word") == True)
	assert(trie.hasPrefix("wor") == True)
	assert(trie.hasPrefix("wort") == False)
	assert(trie.hasPrefix("words") == True)
	assert(trie.hasPrefix("sword") == True)
	assert(trie.hasPrefix("swo") == True)
	assert(trie.hasPrefix("so") == False)



def test_hasPrefix_r():
	trie = Trie()
	assert(trie.hasPrefix("word") == False)

	trie.add("word")
	assert(trie.root.getChildren('w').getChildren('o').getChildren('r').getChildren('d').end_of_word == True)

	trie.add_r("words")
	trie.add_r("sword")

	assert(trie.root.getChildren('w').getChildren('o').getChildren('r').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('w').getChildren('o').getChildren('r').getChildren('d').getChildren('s').end_of_word == True)

	assert(len(trie) == 3)
	assert(trie.hasPrefix_r("word") == True)
	assert(trie.hasPrefix_r("wor") == True)
	assert(trie.hasPrefix_r("wort") == False)
	assert(trie.hasPrefix_r("words") == True)
	assert(trie.hasPrefix_r("sword") == True)
	assert(trie.hasPrefix_r("swo") == True)
	assert(trie.hasPrefix_r("so") == False)

	assert(trie.hasPrefix("word") == True)
	assert(trie.hasPrefix("wor") == True)
	assert(trie.hasPrefix("wort") == False)
	assert(trie.hasPrefix("words") == True)
	assert(trie.hasPrefix("sword") == True)
	assert(trie.hasPrefix("swo") == True)
	assert(trie.hasPrefix("so") == False)



def trie_testcases():
	test_add()
	test_add_r()
	test_hasPrefix()
	test_hasPrefix_r()

# trie testcases




if __name__ == '__main__':
	node_testcases()
	trie_testcases()

