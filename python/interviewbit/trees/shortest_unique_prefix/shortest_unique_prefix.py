'''
https://www.interviewbit.com/problems/shortest-unique-prefix/

Shortest Unique Prefix

Find shortest unique prefix to represent each word in the list.

Example:
Input: [zebra, dog, duck, dove]
Output: {z, dog, du, dov}
	where we can see that
zebra = z
dog = dog
duck = du
dove = dov

NOTE : Assume that no word is prefix of another. In other words, the representation is always possible. 
'''



'''
Solution Outline:
    1. Build a prefix trie of the words.
    2. Start a DFS traversal keeping track of the current prefix from root to current node, 
        and the number of words starting with this prefix
        2.1 If current node at depth,d, has only one word under it => the prefix uniquely identifies a word.
            add current prefix to results and backtrack to previous levels
        2.2 If the current node has > 1 words, continue dfs to lower levels.
	3. Match words to their shortest-unique-prefixes and return in order.

Sample run:
    Input: [zebra, dog, duck, dove]

    trie
      * - z - e - b - r - a
       \- d - o - g
               \- v - e
           \- u - c - k


dfs(*)
 - dfs(z) - has one child => [z]
 - dfs(d) - has two children (o,u)
   - dfs(o) - has two children (g,v)
     - dfs(g) - has one child => [z, dog]
     - dfs(v) - has one child => [z, dog, dov]
   - dfs(u) - has one child => [z, dog, dov, du]
 
'''

class Trie(object):
	class Node(object):
		def __init__(self):
			self.children = {}
			self.eow = False
			self.num_words = 0 # number of words below this prefix

		# return a `child` node from current node
		# towards the path representing 'current node prefix + letter'
		def getChild(self, letter):
			if not self.children.has_key(letter):
				self.children[letter] = Trie.Node()
			return self.children[letter]

	def __init__(self):
		self.root = Trie.Node()

	def add(self, word):
		node = self.root
		for c in word:
			node.num_words += 1
			child = node.getChild(c)
			node = child
		node.eow = True


class Solution:
	def shortest_unique_prefix(self, words):
		def dfs(node, prefix=""):
			if node is None:
				return

			children = node.children.keys()
			if node.num_words == 1 or node.eow:
				sups.append(prefix)
				return

			for n in children:
				dfs(node.getChild(n), prefix+n)


		if len(words) < 2:
			return words

		t = Trie()
		for word in words:
			t.add(word)

		sups = []
		dfs(t.root)

		# match shortest unique prefixes to their words
		sups_ordered = []
		for word in words:
			for prefix in sups:
				if prefix == word[:len(prefix)]:
					sups_ordered.append(prefix)
					break
		return sups_ordered


if __name__ == '__main__':
	t = Trie()
	t.add("abc")
	assert(t.root.getChild('a').getChild('b').getChild('c').eow == True)
	assert(t.root.getChild('a').eow == False)
	assert(t.root.getChild('a').getChild('b').eow == False)
	t.add("acd")
	assert(t.root.getChild('a').getChild('c').getChild('d').eow == True)

	root = t.root
	path_a = t.root.getChild('a')
	assert path_a.num_words == 2
	assert sorted(path_a.children.keys()) == ['b', 'c']

	s = Solution()
	assert s.shortest_unique_prefix(['zebra', 'dog', 'duck', 'dove']) ==\
			['z', 'dog', 'du', 'dov']

	assert s.shortest_unique_prefix([ "zebra", "dog", "duck", "dot" ]) == \
			['z', 'dog', 'du', 'dot']
	assert s.shortest_unique_prefix([ "bearcat", "bert" ]) == ['bea', 'ber']
