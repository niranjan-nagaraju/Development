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
		self.characters = {}
		# indicates if parent node prefix is a whole word
		self.end_of_word = False
		self.frequency = 0 # valid only if end_of_word is true


	def add(self, character):
		if not self.characters.get(character):
			self.characters[character] = Node()


	def remove(self, character, eow=False):
		pass


	def getChildren(self, character):
		return self.characters.get(character)


	def setChildren(self, character):
		self.characters[character] = Node()


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

		trav.eow = True
