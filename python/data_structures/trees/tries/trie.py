'''
Placeholder: Implement a trie
'''


'''
An item in a trie node
Contains
  children: Child node of current item.
  End of Word status: if the sequence of characters at current item is a whole word
  frequency: if current item is a whole word, its frequency of occurence.
  prefix_count: Number of words in the trie that begin with the prefix at current item.
'''
class NodeItem(object):
	def __init__(self):
		self.children = None
		self.end_of_word = False
		self.frequency = 0
		self.prefix_count = 0


'''
Trie node (an n-array tree node)
In a trie, each node contains a map of a character to its node item
A map is used so the entire universe of unicode characters can be used for
storing words in the trie
'''
class Node(object):
	def __init__(self):
		# Each item maps a character 'c' to the next
		# character in a prefix/word, along with its EoW (End of word) status
		# and its frequency of occurence (the number of times it was added to the trie)
		self.items = {}

		# number of words in the trie
		# this should be equivalent to len(self.items)
		# but num_words provides a faster reference
		self.num_words = 0;


	def add(self, character):
		pass

	def __str__(self):
		pass



'''
An item encapsulation that goes inside a Node
Contains a node, EoW status, frequency of occurence if this
item represents the last character in a word added to the trie
'''
class NodeItem(object):
	def __init__(self, node, eow=False, frequency=0):
		self.node = node
		self.isEoW = eow
		self.frequency = frequency

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


	def frequency(self. word):
		pass


	def countPrefix(self, prefix):
		pass


	def findPrefixMatches(self, prefix):
		pass


	# lexicographic sort
	def sorted(self):
		pass


	# remove all words matching prefix
	def removeAllPrefix(self, prefix):
		pass

