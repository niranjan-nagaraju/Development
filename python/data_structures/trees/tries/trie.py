'''
Placeholder: Implement a trie
'''



'''
An item encapsulation that goes inside a Node
Contains a node, EoW status, frequency of occurence if this
item represents the last character in a word added to the trie
'''
class NodeItem(object):
	def __init__(self, node=None, eow=False, frequency=0):
		self.node = node
		self.end_of_word = eow
		self.frequency = frequency



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


	# Length of a node => number of character-keys set in it
	def __len__(self):
		return len(self.items)


	# [] shotcut to get node item from node at 'character' 
	# return None if the node doesn't contain anything at 'key'
	def __getitem__(self, character):
		# TODO: Change this to a decorator later
		if not (isinstance(character, str) or isinstance(character, unicode)):
			raise ValueError("character is neither unicode not utf-8")
		return self.items.get(character)



	# [] shortcut to set node item in node at 'character'
	def __setitem__(self, character, item):
		# TODO: Change this to a decorator later
		if not (isinstance(character, str) or isinstance(character, unicode)):
			raise ValueError("character is neither unicode not utf-8")

		# Nodes are indexed by character-keys
		# and each character-key maps to a node item
		# or None

		# Item is None => reset current node's item at character-key
		if item is None:
			self.items[character] = None

		# Node's character-keys should map to a node item
		if not isinstance(item, NodeItem):
			return

		if not self.items.has_key(character):
			self.items[character] = item
		self.items[character].prefix_count += 1



	# Add an item for character-key 
	def add(self, character):
		item = NodeItem(self)
		self[character] = item



	# remove an item from node at index 'character'
	def remove(self, character):
		if not self[character]:
			return
		self[character] = None



	# return node items and key in current node
	def __str__(self):
		sstr = "[%d]: " %(self.__len__())
		for (character, item) in self.items.items():
			sstr += "(%s, $:%s f:%d pc:%d)" %(character, item.end_of_word, item.frequency, item.prefix_count)
		return sstr.strip()


	# child node for character-key at current node
	def getChild(self, character):
		# character is not set in current node
		if not self[character]:
			return None

		return self[character].node





'''
The Trie class
'''
class Trie(object):
	def __init__(self):
		# number of words added in the trie
		# this should be equivalent to len(self.items)
		# but num_words provides a faster reference
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

