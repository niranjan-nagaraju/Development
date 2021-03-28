'''
A trie implementation

Design:
	Implemented as an n-ary tree.
	The root can have 'n' keys -> {data, childnodes} entries.
	If a node (corresponding to prefix, p) has key, k1, then there exists a path/word `p+k1` in the trie.

	word, 'abcd' utilizes exactly 4 nodes.
	  * -> a -> b -> c -> d  (eow status is stored in the associated key's entry)
	This makes it slightly more memory-efficient compared to the `simple-trie` which uses an additional empty-node to mark eow status(es).
'''


class Trie(object):
	class Node(object):
		def __init__(self):
			self.keys = {}

		# return `nodeItem` at keys[key]
		# return `None` if key is not in the dictionary
		def __getitem__(self, key):
			return self.keys.get(key)

		# set `nodeItem` at keys[key]
		# raise `KeyError` if key is not in the dictionary
		def __setitem__(self, key, nodeItem):
			self.keys[key] = nodeItem

	class NodeItem(object):
		def __init__(self, data=None):
			self.data = data
			self.eow = False
			self.child = None

	def __init__(self):
		self.root = Trie.Node()

	def add(self, word, data=None):
		node = self.root
		parent = None
		for c in word:
			if node is None:
				node = Trie.Node()
				parent.child = node

			nodeItem = node[c]
			if nodeItem is None:
				nodeItem = Trie.NodeItem()
				node[c] = nodeItem

			parent = nodeItem
			node = nodeItem.child

		parent.eow = True
		parent.data = data
