'''
placeholder for a simple trie implementation

Design:
	Implemented as an n-ary tree.
	The root has 'n' nodes (n being the number of characters in the universe),
	  root['A'] -> child node, indicates there is a word that begins with the character, 'A'
	  the child node's properties, such as eow indicates eow status of 'A'
	  Additional properties such as frequency can be added to the child nodes.
'''

class Node:
	def __init__(self):
		self.children = {}
		# indicates if parent node prefix is a whole word
		self.end_of_word = end_of_word


	def add(self, character, eow=False):
		if not self.children[character]:
			self.children[character] = Node()
		self.children[character].end_of_word = eow


	def remove(self, character, eow=False):
		pass
