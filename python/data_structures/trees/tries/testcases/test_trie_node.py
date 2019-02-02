import sys
sys.path.append("../../../../")
from data_structures.trees.tries.trie import Node, NodeItem


def testcase1():
	node = Node()
	try:
		node[-1] = 100
	except ValueError as e:
		assert(e.message == "ValueError: '__setitem__(): character is neither unicode nor ascii'")

	assert(node.items == {})
	assert(len(node) == 0)

	node['A'] = NodeItem(None, eow=False, frequency=2, prefix_count=1)
	assert(len(node) == 1)
	assert(node['A'].children == None)
	assert(node['A'].end_of_word == False)
	assert(node['A'].frequency == 2)
	assert(node['A'].prefix_count == 2) # adding a node item updates prefix count

	node2 = Node()
	node2['B'] = NodeItem(None, eow=True, frequency=1, prefix_count=0)
	assert(len(node2) == 1)
	node['A'].children = node2
	assert(node['A'].children == node2)
	assert(node2['B'].children == None)
	assert(node2['B'].end_of_word == True)
	assert(node2['B'].frequency == 1)
	assert(node2['B'].prefix_count == 1) # adding a node item updates prefix count




if __name__ == "__main__":
	testcase1()

