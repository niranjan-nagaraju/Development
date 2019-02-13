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




# trie node testcases

# trie testcases
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

	trie.add("abc")
	assert(trie.num_words == 2)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 1)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').getChildren('d').frequency == 1)

	trie.add("abc") # Increase frequency
	assert(trie.num_words == 2)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').end_of_word == True)
	assert(trie.root.getChildren('a').getChildren('b').getChildren('c').frequency == 2)



def trie_testcases():
	test_add()

# trie testcases




if __name__ == '__main__':
	node_testcases()
	trie_testcases()
