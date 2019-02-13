import sys
sys.path.append("../../../../")
from data_structures.trees.tries.trie import Node, NodeItem


def test_basic():
	node = Node()
	try:
		node[-1] = 100
	except ValueError as e:
		assert(e.message == "ValueError: '__setitem__(): character is neither unicode nor ascii'")

	assert(node.items == {})
	assert(len(node) == 0)
	assert(not node == True)

	node['A'] = NodeItem(None, eow=False, frequency=2)
	assert(repr(node) == "[1]: ('A', $:False f:2)")
	assert(str(node) == "[1]: ['A']")
	assert(len(node) == 1)
	assert(node['A'].children == None)
	assert(node['A'].end_of_word == False)
	assert(node['A'].frequency == 2)

	node2 = Node()
	node2.add('B')
	assert(node2['B'] is not None)
	node2['B'].frequency = 1
	node2['B'].end_of_word = True
	assert(repr(node2) == "[1]: ('B', $:True f:1)")
	assert(str(node2) == "[1]: ['B']")
	assert(len(node2) == 1)
	node['A'].children = node2
	assert(node['A'].children == node2)
	assert(node2['B'].children == None)
	assert(node2['B'].end_of_word == True)
	assert(node2['B'].frequency == 1)

	node2.add('C')
	assert(node2['C'] is not None)
	node2['C'].frequency = 1
	node2['C'].end_of_word = True
	assert(repr(node2) == "[2]: ('B', $:True f:1) ('C', $:True f:1)")
	assert(str(node2) == "[2]: ['B', 'C']")
	assert(len(node2) == 2)
	assert(node['A'].children == node2)
	assert(node2['C'].children == None)
	assert(node2['C'].end_of_word == True)
	assert(node2['C'].frequency == 1)


def test_remove():
	node = Node()
	node.add('a')
	node.add('b')
	assert(len(node) == 2)
	node.remove('a')
	assert(len(node) == 1)
	node.remove('b')
	assert(len(node) == 0)


if __name__ == "__main__":
	test_basic()
	test_remove()

