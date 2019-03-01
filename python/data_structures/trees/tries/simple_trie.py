'''
A simple trie implementation

Design:
	Implemented as an n-ary tree.
	The root has 'n' nodes (n being the number of characters in the universe),
	  root['A'] -> child node, indicates there is a word that begins with the character, 'A'
	  the child node's properties, such as eow indicates eow status of 'A'
	  Additional properties such as frequency can be added to the child nodes.
'''



'''
Node class for the trie
Each node contains:
	children: A mapping of characters -> their child nodes
	data: user data (valid only if eow is set)
	end_of_word: if characters in the path to parent node is a whole word.
	frequency: frequency of occurence of a word (valid only if eow is set)
'''
class Node(object):
	def __init__(self):
		self.children = {}
		self.data = None
		# indicates if parent node prefix is a whole word
		self.end_of_word = False
		self.frequency = 0 # valid only if end_of_word is true


	'''
	length of a node is the number of characters
	set in the node
	'''
	def __len__(self):
		return len(self.children)


	'''
	Node str(): returns [size]: [characters set in node]
	'''
	def __str__(self):
		return "[%d]: %s" %(len(self), sorted(self.children.keys()))


	'''
	Node repr(): returns [size]: [characters set in node], eow: <T/F>, frequency: <f> 
	'''
	def __repr__(self):
		return "%s: " %(self) + ", eow: %s, frequency: %d" %(self.eow, self.frequency) 


	'''
	Add a character in the node
	If the character is already set, just return without doing nothing
	'''
	def add(self, character):
		if self.children.get(character) is None:
			self.children[character] = Node()


	'''
	Remove a character from the node
	if the character has a child node that's not empty, then do nothing
	   => character is part of a prefix and there are still words
	   with character as part of their prefix
	'''
	def remove(self, character):
		node = self.children.get(character)
		# character is not in the node
		if node is None:
			return None

		# character has an empty child node
		# un-set the character from the node
		# and remove the empty child node
		if not node:
			self.children[character] = None
			self.children.pop(character)

		# return the child node
		return node

		

	'''
	Get child node of current node at character
	'''
	def getChildren(self, character):
		return self.children.get(character)


	'''
	set a new child node of current node at character
	'''
	def setChildren(self, character):
		self.children[character] = Node()


	'''
	getter for EoW status
	'''
	@property
	def eow(self):
		return self.end_of_word


	'''
	setter for EoW status
	this also updates frequency based on whether
	EoW is being set to true/false
	'''
	@eow.setter
	def eow(self, end_of_word):
		if not isinstance(end_of_word, bool):
			raise TypeError("eow(): end of word should be a boolean value")
		self.end_of_word = end_of_word
		if end_of_word == True:
			self.frequency += 1
		else:
			self.frequency -= 1




class Trie(object):
	def __init__(self):
		self.root = None
		self.num_words = 0


	def __len__(self):
		if not self.root:
			return 0
		return self.num_words


	'''
	add a word to the trie
	If data is provided for word, add that too
	'''
	def add(self, word, data=None):
		if not self.root:
			self.root = Node()

		trav = self.root
		for c in word:
			trav.add(c)
			trav = trav.getChildren(c)

		# first time we are adding the word, increase number of words
		if not trav.eow:
			self.num_words += 1

		# mark EoW and increase frequency
		trav.eow = True
		trav.data = data


	'''
	Recursive version
	add a word to the trie
	If data is provided for word, add that too
	'''
	def add_r(self, word, data=None):
		def add_helper(root, word, data=None):
			# we have added all the characters in the word
			# Mark is EoW, set its 'data' field and return
			if not word:
				# first time we are adding the word, increase number of words
				if not root.eow:
					# trie's reference is obtained outer function
					self.num_words += 1

				# mark EoW and increase frequency
				root.eow = True
				root.data = data
				return

			root.add(word[0])
			root = root.getChildren(word[0])
			add_helper(root, word[1:], data)

		# Empty word, cannot be added to the trie
		if not word:
			return

		# first word to be added to the trie
		# initialize root
		if not self.root:
			self.root = Node()

		# call helper function to add word
		add_helper(self.root, word, data)




	'''
	Return if 'prefix' is a prefix to one or more
	words in the trie
	'''
	def hasPrefix(self, prefix):
		if not self.root:
			return False

		trav = self.root
		for p in prefix:
			tmp = trav.getChildren(p)
			if tmp is None:
				return False
			trav = tmp

		return True


	'''
	Recursive version:
	Return if 'prefix' is a prefix to one or more
	words in the trie
	'''
	def hasPrefix_r(self, prefix):
		def hasPrefix_helper(root, prefix):
			# couldn't match prefix
			if root is None:
				return False
			
			if not prefix:
				return True

			return hasPrefix_helper(root.getChildren(prefix[0]), prefix[1:])

		if not self.root or not prefix:
			return False

		return hasPrefix_helper(self.root, prefix)

