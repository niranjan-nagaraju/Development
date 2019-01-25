'''
Placeholder: Implement a trie
'''

'''
Trie node (an n-array tree node)
In a trie, each node contains '128' child nodes, one for each printable ascii
character
'''
class Node(object):
	# Assumes the letters allowed are printable ascii [0-127]
	MAX_CHARACTERS_IN_UNIVERSE = 128
	def __init__(self):
		# child nodes
		self.children = [None] * MAX_CHARACTERS_IN_UNIVERSE

		# Is there a word that ends in a character at current node
		self.isEndOfWord = False

		# A running count of the number of words in the trie
		# that start with the prefix that the current node represents
		self.prefix_count; 


	def addChild(self, character):
		pass

	def __str__(self):
		pass


'''
The Trie class
'''
class Trie(object):
	def __init__(self):
		# number of words added to the trie
		self.num_words = 0
		self.root = None

	def __len__(self):
		return self.num_words

	def __nonzero__(self):
		return (self.root == None)


	def add(self, word):
		pass


	def remove(self, word):
		pass


	def hasWord(self, word):
		pass


	def hasPrefix(self, prefix):
		pass


	def countPrefix(self, prefix):
		pass


	def findPrefixMatches(self, prefix):
		pass

