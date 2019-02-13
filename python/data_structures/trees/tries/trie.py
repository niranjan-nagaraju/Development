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
	  pc: prefix count: number of words that begin the prefix at current node (!!: cannot be maintained across prefix removals)


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
NOTE: prefix_count cannot be accurately maintained in an node item
      deleting all words that match a prefix for e.g., will need upstream
	  propagation of how many words were deleted for each prefixes of length 1 to specified prefix.
'''
class NodeItem(object):
	def __init__(self, children=None, eow=False, frequency=0):
		self.children = children
		self.end_of_word = eow
		self.frequency = frequency


	def __str__(self):
		return "children:%s eow:%s freq:%d" %(self.children, self.end_of_word,
				self.frequency)



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
		return len(self.items) if self.items else 0



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

		# Node's character-keys should map to a node item
		if not (isinstance(item, NodeItem) or item is None):
			return

		self.items[character] = item



	# Add an item for character-key 
	def add(self, character):
		if not self[character]:
			self[character] = NodeItem()



	# remove an item from node at index 'character'
	def remove(self, character):
		if not self[character]:
			return

		# remove child node link
		self[character].children = None

		# set item to None so it can be garbage-collected?!
		self[character] = None
		# remove character from the current node's hash table
		self.items.pop(character)

		# if we just removed the last character set in the node,
		# 'empty' the node as well
		# NOTE: Is this needed?
		if not self:
			self.items = None





	# return node items and key in current node
	def __repr__ (self):
		sstr = "[%d]: " %(self.__len__())
		for (character, item) in sorted(self.items.items()):
			sstr += "(%r, $:%s f:%d) " %(character, item.end_of_word, item.frequency)
		return sstr.strip()


	def __str__(self):
		return "[%d]: %s" %(self.__len__(), sorted(self.items.keys()))



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
			if self.root is None:
				raise TrieEmptyError("TrieEmptyError: '%s(): Trie is empty'" %(func.__name__))
			rv = func(self, *args)
			return rv
		return f


	# Return the last node in the trie for a matching prefix
	# if it exists, 
	# if it doesn't, return None
	def findMatchingPrefixNode(self, prefix):
		if not prefix:
			return None

		trav = self.root

		last = trav;
		for c in prefix:
			if not trav or not trav[c]:
				return None
			last = trav
			trav = trav[c].children

		# trav is one level below "prefix", and therefore one level too far.
		# last is at the end node containing prefix.
		return last


	# Return the last node item in the trie for a matching prefix
	# if it exists, 
	# if it doesn't, return None
	def findMatchingPrefixNodeItem(self, prefix):
		last = self.findMatchingPrefixNode(prefix)
		return last[prefix[-1]] if last else None



	# Add a word to the trie
	# if the word already exists in the trie,
	#  just update its frequency and return
	def add(self, word):
		# First word to be added to the trie
		# create a root node
		if not self.root:
			self.root = Node()

		# Update frequency if word exists
		# and return
		# NOTE: Creating 'root' before looking for the word, also ensures 
		# check_root() passes
		item = self.findMatchingPrefixNodeItem(word)
		if item and item.end_of_word == True:
			item.frequency += 1
			return

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



	# remove a word from the trie
	@check_root
	def remove(self, word):
		# Helper function to recursively remove
		# word from the bottom-up
		def removehelper(node, word):
			# we have either reached 'n' levels deep
			# n: len(word)
			# or word was "" to begn with
			# in which case, there's nothing to remove anyway
			if not word:
				return True

			# word search ended prematurely
			# before we could match it
			# e.g. trie has 'ab', and we are looking for 'abc'
			if not node:
				return False

			item = node[word[0]]
			# couldn't match word completely
			if not item:
				return False

			if len(word) == 1:
				if not item.end_of_word:
					return False
				item.end_of_word = False

			# recursively try to remove child nodes
			# if a child node remove succeeds, check if current node
			# can safely un-set character,
			# if un-setting the character in current node makes the node empty
			# remove the current node too
			if removehelper(item.children, word[1:]):
				# child node was matched and 'removed'

				# Either we are at the last character in the word,
				# or removing the next character in the word rendered the
				# node empty (i.e. no characters set in the child node)
				if (not item.children):
					node.remove(word[0])
				return True

			# one of the child nodes returned false
			# so the word couldn't be matched completely
			return False


		# empty word
		if not word:
			return False

		# word was found in the trie
		# and removed using the helper function
		# update number of words in the trie
		# and adjust root if the trie becomes empty
		# post-removal
		if removehelper(self.root, word):
			self.num_words -= 1
			# this was the last word to be removed from the trie
			# and the trie is now empty
			if not len(self.root):
				self.root = None
			return True

		# helper function did not remove the word
		# either due to word not being found in the trie
		# or if it's not completely matched (as a whole word)
		return False




	# helper function to remove all child nodes below specified node
	def removeChildNodes(self, node):
		if not node:
			return

		for (character, item) in node.items.items():
			self.removeChildNodes(item.children)

			# if the last character we removed is a word,
			# update word count in the trie
			if node[character].end_of_word:
				self.num_words -= 1
			node.remove(character)



	# remove all words matching prefix
	@check_root
	def removePrefix(self, prefix):
		# If an empty prefix is specified
		# delete the whole trie
		if not prefix:
			self.removeChildNodes(self.root)
			self.root = None
			# individually removing every character should also
			# have deducted word count by exactly the
			# length of the trie
			assert(self.num_words == 0)

		# Get node containing last character in the prefix
		last_node = self.findMatchingPrefixNode(prefix)
		# couldn't match prefix in the trie
		if not last_node:
			return

		# call helper function to remove all child nodes of 'last node'
		self.removeChildNodes(last_node[prefix[-1]].children)

		# If prefix by itself is also a word,
		# update word count in the trie accordingly
		# as the prefix (/word) willl also be removed from the trie
		if last_node[prefix[-1]].end_of_word:
			self.num_words -= 1

		# remove last character in prefix from the trie as well
		# e.g. 'ab*' removes * first, and then removes 'b' from as well
		last_node.remove(prefix[-1])






	# Return true if the trie has 'word'
	# false otherwise
	@check_root
	def hasWord(self, word):
		item = self.findMatchingPrefixNodeItem(word)
		return item.end_of_word if item else False


	# Return true if the trie has 'prefix'
	# false otherwise
	@check_root
	def hasPrefix(self, prefix):
		item = self.findMatchingPrefixNodeItem(prefix)
		return (item is not None)


	# Return word frequency if the trie has 'word'
	# 0 otherwise
	@check_root
	def frequency(self, word):
		item = self.findMatchingPrefixNodeItem(word)
		if item and item.end_of_word:
			return item.frequency
		return 0



	# Return number of words that match a prefix
	# if the trie has words that begin with 'prefix'
	# 0 if no such prefix exists
	@check_root
	def countPrefix(self, prefix):
		return len(self.prefixMatches(prefix))



	# Returns a list of words that match a prefix
	# if the trie has words that begin with 'prefix'
	# None if no such prefix exists
	# raises 'TrieEmptyError' if trie is not initialized
	@check_root
	def prefixMatches(self, prefix=None):
		# DFS search from the matching prefix node 
		# Return a list of words that start with prefix
		def dfs_search_helper(node, words_list, prefix=""):
			if not node:
				return

			# look for characters set at the current node
			# sort by keys for a lexicographic order
			for (c , item) in sorted(node.items.items()):
				# extend the prefix by the current character
				if item:
					words_list.append(prefix + c) if item.end_of_word else None
					dfs_search_helper(item.children, words_list, prefix + c)


		# call the dfs helper
		words_list = []
		# empty prefix -> match everything in the trie
		if not prefix:
			dfs_search_helper(self.root, words_list)
		else:
			item = self.findMatchingPrefixNodeItem(prefix)
			# Couldn't find the prefix in the trie,
			# return an empty list
			if not item:
				return words_list

			# prefix is a whole word by itself
			# add it to the list of matching words
			if item.end_of_word:
				words_list.append(prefix)
			dfs_search_helper(item.children, words_list, prefix)

		return words_list




	# lexicographic sorted sequence of words in the trie
	def sorted(self):
		return self.prefixMatches("")



