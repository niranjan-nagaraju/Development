'''
placeholder for a simple trie implementation

Design:
	Implemented as an n-ary tree.
	The root has 'n' nodes (n being the number of characters in the universe),
	  root['A'] -> child node, indicates there is a word that begins with the character, 'A'
	  the child node's properties, such as eow indicates eow status of 'A'
	  Additional properties such as frequency can be added to the child nodes.
'''

class Node(object):
	def __init__(self):
		self.children = {}
		# indicates if parent node prefix is a whole word
		self.end_of_word = False
		self.frequency = 0 # valid only if end_of_word is true


	# length of a node is the number of characters
	# set in the node
	def __len__(self):
		return len(self.children)


	def __str__(self):
		return "[%d]: %s" %(len(self), sorted(self.children.keys()))


	def __repr__(self):
		return "%s: " %(self) + ", eow: %s, frequency: %d" %(self.eow, self.frequency) 


	# Add a character in the node
	# If the character is already set, just return without doing nothing
	def add(self, character):
		if not self.children.get(character):
			self.children[character] = Node()



	# Remove a character from the node
	# if the character has a child node that's not empty, then do nothing
	#    => character is part of a prefix and there are still words
	#    with character as part of their prefix
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

		

	# Get child node of current node at character
	def getChildren(self, character):
		return self.children.get(character)


	# set a new child node of current node at character
	def setChildren(self, character):
		self.children[character] = Node()


	@property
	def eow(self):
		return self.end_of_word


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


	def add(self, word):
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
