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


	def __nonzero__(self):
		return True

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
		if not self.children.get(character):
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
		if not node.children:
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
		self.root = None
		self.num_words = 0


	def __len__(self):
		if not self.root:
			return 0
		return self.num_words


	'''
	decorator to ensure root is not empty
	on trie operations
	'''
	def check_root(func):
		def f(self, *args, **kwargs):
			if self.root is None:
				raise TrieEmptyError("TrieEmptyError: '%s(): Trie is empty'" %(func.__name__))
			rv = func(self, *args, **kwargs)
			return rv
		return f


	'''
	Find a node matching the path of characters in the 
	prefix, inside the trie
	'''
	@staticmethod
	def findMatchingNode(root, prefix):
		if not root or not prefix:
			return None

		trav = root
		for p in prefix:
			trav = trav.getChildren(p)
			if not trav:
				return None

		return trav


	'''
	Recursive version:
	Find a node matching the path of characters in the 
	prefix, inside the trie
	'''
	@staticmethod
	def findMatchingNode_r(root, prefix):
		if not root:
			return None

		if not prefix:
			return root

		return Trie.findMatchingNode_r(root.getChildren(prefix[0]), prefix[1:])


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
			add_helper(root.getChildren(word[0]), word[1:], data)

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
		node = self.findMatchingNode(self.root, prefix)
		return (node is not None)


	'''
	Recursive version:
	Return if 'prefix' is a prefix to one or more
	words in the trie
	'''
	def hasPrefix_r(self, prefix):
		node = self.findMatchingNode_r(self.root, prefix)
		return (node is not None)




	'''
	Return if 'word' is present as a whole word in the trie
	'''
	def hasWord(self, word):
		node = self.findMatchingNode(self.root, word)
		return node.eow if node else False



	'''
	Recursive version
	Return if 'word' is present as a whole word in the trie
	'''
	def hasWord_r(self, word):
		node = self.findMatchingNode_r(self.root, word)
		return node.eow if node else False



	'''
	Calculate frequency of occurence of a certain word in the trie
	(the number of times it was added)
	NOTE: frequency is only valid for whole words
	'''
	def frequency(self, word):
		node = self.findMatchingNode(self.root, word)
		if not node or not node.eow:
			return 0

		return node.frequency


	'''
	Recursive version
	Calculate frequency of occurence of a certain word in the trie
	(the number of times it was added)
	NOTE: frequency is only valid for whole words
	'''
	def frequency_r(self, word):
		node = self.findMatchingNode_r(self.root, word)
		if not node or not node.eow:
			return 0

		return node.frequency



	'''
	[] syntax:
	Return word's data if the trie has 'word'
	raise KeyError otherwise
	'''
	@check_root
	def __getitem__(self, word):
		node = self.findMatchingNode(self.root, word)
		if node and node.eow:
			return node.data
		else:
			raise KeyError("__getitem__(): Word %r not found in trie" %(word))


	'''
	[] syntax:
	Set word's data if the trie has 'word', replacing any value it might already have
	if the word doesn't exist in the trie, add it now
	'''
	@check_root
	def __setitem__(self, word, value=None):
		node = self.findMatchingNode(self.root, word)
		if node and node.eow:
			node.data = value
		else:
			self.add(word, value)



	'''
	Traverse the Trie depth-first, starting from a root node (representing a prefix)
	en-listing all matching words (and associated data) in the trie that start with the prefix.
	en-listing => call a function callback to allow a caller to specify
		what needs to be done with individual entries
	'''
	def dfs(self, prefix, fn, *args, **kwargs):
		def dfs_helper(root, prefix, fn, *args, **kwargs):
			if not root:
				return

			for (c,node) in sorted(root.children.items()):
				fn(prefix+c, node.data, *args, **kwargs) if node.eow else None
				dfs_helper(node, prefix+c, fn, *args, **kwargs)

		if not prefix:
			node = self.root
		else:
			node = self.findMatchingNode(self.root, prefix)
			if not node:
				return

		fn(prefix, node.data, *args, **kwargs) if node.eow else None
		dfs_helper(node, prefix, fn, *args, **kwargs)



	'''
	Returns a list of words that match a prefix
	if the trie has words that begin with 'prefix'
	None if no such prefix exists
	raises 'TrieEmptyError' if trie is not initialized
	'''
	@check_root
	def findKeys(self, prefix=""):
		keys = []
		self.dfs(prefix, lambda a,_,k: k.append(a), keys)
		return keys



	'''
	Returns a list of (words, value) that match a prefix
	if the trie has words that begin with 'prefix'
	None if no such prefix exists
	raises 'TrieEmptyError' if trie is not initialized
	'''
	@check_root
	def search(self, prefix=""):
		matches = []
		self.dfs(prefix, lambda a,b,k: k.append((a,b)), matches)
		return matches



	'''
	Return number of words that match a prefix
	if the trie has words that begin with 'prefix'
	0 if no such prefix exists
	'''
	@check_root
	def count(self, prefix):
		def _countfn(a, b, counter):
			counter[0] += 1

		counter = [0]
		self.dfs(prefix, _countfn, counter)
		return counter[0]


	'''
	remove a word from the trie
	Return its assciated data upon successful removal

	decrease frequency until it becomes 0,
	if frequency hits 0, then remove the word from the trie
	unless 'forceremove' is set, in which case, remove the word
	unconditionally.
	'''
	@check_root
	def remove(self, word, forceremove=False):
		# Helper function to recursively remove
		# word from the bottom-up
		def removehelper(node, word):
			if not node:
				# word search ended prematurely
				# before we could match it
				# e.g. trie has 'ab', and we are looking for 'abc'
				return False

			# we have either reached 'n' levels deep
			# n: len(word)
			# or word was "" to begn with
			# in which case, there's nothing to remove anyway
			if not word:
				node.eow = False
				node.data = None
				return True

			if removehelper(node.children[word[0]], word[1:]):
				# child node was matched and 'removed'
				# remove link to child node
				node.remove(word[0])

				# one of the prefixes is a word in itself
				# do not remove this whole path
				# return false from here, so upper nodes will see they have a child left
				# and wont remove themselves
				if node.eow:
					return False

				# communicate to higher levels that word was matched
				# and 'deleted'
				# if word had children, or if its prefixes were words themselves
				# then the entire path wouldn't be removed
				return True

			# one of the child nodes returned false
			# so the word couldn't be matched completely
			return False

	
		node = self.findMatchingNode(self.root, word)
		if not node or not node.eow:
			return None

		data = node.data

		node.frequency -= 1
		if forceremove or node.frequency == 0:
			removehelper(self.root, word)
			self.num_words -= 1
			# this was the last word to be removed from the trie
			# and the trie is now empty
			if not len(self.root):
				self.root = None
			return data

		return None



	'''
	helper function to remove all child nodes below specified node
	'''
	def removeChildNodes(self, node):
		if not node:
			return

		if node.end_of_word:
			node.eow = False
			self.num_words -= 1

		for (character, cnode) in node.children.items():
			self.removeChildNodes(cnode)

			# if the last character we removed is a word,
			# update word count in the trie
			node.remove(character)



	'''
	remove all words matching prefix
	'''
	@check_root
	def removePrefix(self, prefix=""):
		# If an empty prefix is specified
		# delete the whole trie
		if not prefix:
			self.removeChildNodes(self.root)
			self.root = None
			# individually removing every character should also
			# have deducted word count by exactly the
			# length of the trie
			assert(self.num_words == 0)


		# stop at last-but-one node, unlink this node's link to the child node at character
		# then remove everything below and including the child node
		if len(prefix) == 1:
			pnode = self.root
		else:
			# Get node containing last character in the prefix
			pnode = self.findMatchingNode(self.root, prefix[:-1])

		# couldn't match prefix in the trie
		if not pnode:
			return

		# if prefix itself is a word, remove it first
		node = pnode.children[prefix[-1]]
		if not node:
			return

		# cut off link to all child nodes following the prefix-node, un-set its last character link in the parent node
		pnode.remove(prefix[-1])

		# call helper function to remove all child nodes of 'node'
		self.removeChildNodes(node)



	'''
	Remove everything in the trie
	'''
	def destroy(self):
		self.removePrefix()

