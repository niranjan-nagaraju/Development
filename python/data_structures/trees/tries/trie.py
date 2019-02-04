'''
Placeholder: Implement a trie

Design:
	The trie is built as an n-array tree.
	Each node contains a mapping for a character to its child nodes, and a few other node items.
	The root node represents the first character of the words added to the trie,
	 second level nodes contain the second character, and so on.
	Words are added to the trie by following a path from the root, and navigating to the child node corresponding to the current character at position i to level i.
	A node at level i, represents a word/prefix, i.e. a sequence of characters of size 'i'  containing the characters in the path from root to current node.
	(ofcourse, a trie being a tree, each node has only one parent)

	Each node contains the following node items, for each 'allowed' character
	  $, end-of-word status: that says if there is a word that ends at current node (from the path of characters starting from root)
	  f, frequency of occurence of the word (valid only if $ is set)
	  pc: prefix count: number of words that begin the prefix at current node.


e.g.,
  the following trie contains the words:
    A  (with a frequency of 9)
    AB (with a frequency of 2)
	CA (with a frequency of 1)

	Two words that begin with A (pc: 2) -> A, AB
	One word that begins with C (pc: 1) -> CA


Node(root)
|-------+-------+-------|
|  A    |  B    |  C    |
|-------+-------+-------|Items
| f:  9 | f:  0 | f:  0 |
| pc: 2 | pc: 0 | pc: 1 +----------+
| $:  1 | $:  0 | $:  0 |          |
|---+----+-------+------|          |
    |                              |
    +------+                       |
Node       |                       |                 Node     
|-------+--v----+-------|       |--v-----+-------+------|    
|  A    |  B    |  C    |       |  A    |  B    |  C    |
|-------+-------+-------|       |-------+-------+-------|
| f:  0 | f:  2 | f:  0 |       | f:  1 | f:  0 | f:  0 |
| pc: 0 | pc: 1 | pc: 0 |       | pc: 1 | pc: 0 | pc: 0 |
| $:  0 | $:  1 | $:  0 |       | $:  1 | $:  0 | $:  0 |
|-------+-------+-------|       |-------+-------+-------|
Items                                               Items

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
	def __init__(self, children=None, eow=False, frequency=0, prefix_count=0):
		self.children = children
		self.end_of_word = eow
		self.frequency = frequency
		self.prefix_count = prefix_count



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



	# decorator to ensure character is
	# either unicode as an ascii
	def check_character(func):
		def f(self, character, *args):
			if not (isinstance(character, str) or isinstance(character, unicode)):
				raise ValueError("ValueError: '%s(): character is neither unicode nor ascii'" %(func.__name__))
			rv = func(self, character, *args)
			return rv
		return f



	# [] shortcut to get node item from node at 'character' 
	# return None if the node doesn't contain anything at 'key'
	@check_character
	def __getitem__(self, character):
		return self.items.get(character)



	# [] shortcut to set node item in node at 'character'
	@check_character
	def __setitem__(self, character, item):
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
		item = NodeItem()
		self[character] = item



	# remove an item from node at index 'character'
	def remove(self, character):
		if not self[character]:
			return
		self[character] = None



	# return node items and key in current node
	def __repr__ (self):
		sstr = "[%d]: " %(self.__len__())
		for (character, item) in self.items.items():
			sstr += "(%r, $:%s f:%d pc:%d) " %(character, item.end_of_word, item.frequency, item.prefix_count)
		return sstr.strip()


	def __str__(self):
		return "[%d]: %s" %(self.__len__(), self.items.keys())



'''
Trie Empty exception
'''
class TrieEmptyError(Exception):
	def __init__(self, message='Trie is empty!'):
		self.message = message

	def __str__(self):
		return self.message



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


	# length of the trie = number of words in the trie
	def __len__(self):
		return self.num_words


	# for if trie / (not trie) syntax to succeed
	def __nonzero__(self):
		return (self.root != None)


	# decorator to ensure root is not empty
	# on trie operations
	def check_root(func):
		def f(self, *args):
			if not self.root:
				raise TrieEmptyError("TrieEmptyError: '%s(): Trie is empty'" %(func.__name__))
			rv = func(self, *args)
			return rv
		return f


	
	# TODO partial implementation - complete
	def add(self, word):
		# TODO: Update frequency if word exists
		# and return

		# First word to be added to the trie
		# create a root node
		if not self.root:
			self.root = Node()

		trav = self.root
		trav.add(word[0])
		p = word[0] # previous character
		for c in word[1:]:
			child = trav[p].children

			# parent node doesn't have a child node at character p
			if not child:
				child = Node()
				# set parent node's child node at previous character p
				trav[p].children = child

			# At this point, we have a child node for p from parent node
			# Set current character,c, in child node
			child.add(c)

			# move one level down to current character's node
			trav = child
			p = c

		# set EoW status, and frequency at the last character of the word
		trav[p].end_of_word = True
		trav[p].frequency += 1

		# update number of words in the trie
		self.num_words += 1



	def remove(self, word):
		pass


	@check_root
	def hasWord(self, word):
		if not word:
			return False

		try:
			trav = self.root
		except TrieEmptyError:
			return False

		last = trav;
		for c in word:
			if not trav or not trav[c]:
				return False
			last = trav
			trav = trav[c].children

		# trav is one level below "prefix", and therefore one level too far.
		# last is at the end node containing prefix.
		return last[word[-1]].end_of_word


	def hasPrefix(self, prefix):
		pass


	def frequency(self, word):
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

