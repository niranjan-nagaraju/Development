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
Solution Outline: (Optimized trie - do no create empty trie nodes for storing eow)
    1. Build a prefix trie of the words.
    2. Start a DFS traversal keeping track of the current prefix from root to current node, 
        Store the number of words starting with this prefix, and the word(s) matching this prefix
        2.1 If current node at depth,d, has only one word under it => the prefix uniquely identifies a word.
            add (current prefix, word) to results and backtrack to previous levels
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
	class NodeItem(object):
		def __init__(self):
			self.link = None
			self.eow = False
			self.words_idx = [] # words that match this prefix


	class Node(object):
		def __init__(self):
			# a mapping of character -> its nodeitem
			self.mapping = {}

		def keys(self):
			return self.mapping.keys()

		def __getitem__(self, letter):
			ni = self.mapping.get(letter)
			if ni is None:
				ni = Trie.NodeItem()
				self.mapping[letter] = ni
			return ni

		def getChildNode(self, letter):
			return self[letter].link


	def __init__(self):
		self.root = Trie.Node()

	def add(self, word, idx):
		node = self.root
		prev_ni = None
		for c in word:
			if node is None:
				node = Trie.Node()
				prev_ni.link = node
			ni = node[c]
			ni.words_idx.append(idx)
			prev_ni = ni
			node = ni.link
		prev_ni.eow = True


class Solution:
	def shortest_unique_prefix(self, words):
		def dfs(node, prefix=""):
			if node is None:
				return

			children = node.keys()
			for c in children:
				ni = node[c]
				if len(ni.words_idx) == 1 or ni.eow:
					sups[ni.words_idx[0]] = prefix+c
					continue
				dfs(ni.link, prefix+c)


		if len(words) < 2:
			return words

		t = Trie()
		for idx,word in enumerate(words):
			t.add(word, idx)


		sups = [None] * len(words)
		dfs(t.root)
		
		return sups


if __name__ == '__main__':
	t = Trie()
	t.add("abc", 0)

	assert t.root.keys() == ['a']
	assert t.root.getChildNode('a').keys() == ['b']
	assert t.root.getChildNode('a').getChildNode('b').keys() == ['c']
	assert t.root.getChildNode('a').getChildNode('b').getChildNode('c') == None

	assert t.root['a'].eow == False
	assert t.root['a'].words_idx == [0]
	assert t.root.getChildNode('a')['b'].eow == False
	assert t.root.getChildNode('a')['b'].words_idx == [0]
	assert t.root.getChildNode('a').getChildNode('b')['c'].eow == True
	assert t.root.getChildNode('a').getChildNode('b')['c'].words_idx == [0]

	n1 = t.root.getChildNode('a')
	t.add("acd", 1)
	assert t.root.getChildNode('a') == n1

	assert t.root.keys() == ['a']
	assert sorted(t.root.getChildNode('a').keys()) == ['b', 'c']
	assert t.root.getChildNode('a').getChildNode('c').keys() == ['d']
	assert t.root.getChildNode('a').getChildNode('c').getChildNode('d') == None

	assert t.root.getChildNode('a').getChildNode('c')['d'].eow == True
	assert t.root['a'].words_idx == [0,1]
	assert t.root.getChildNode('a')['c'].words_idx == [1]

	s = Solution()
	assert s.shortest_unique_prefix(['zebra', 'dog', 'duck', 'dove']) ==\
			['z', 'dog', 'du', 'dov']

	assert s.shortest_unique_prefix([ "zebra", "dog", "duck", "dot" ]) == \
			['z', 'dog', 'du', 'dot']
	assert s.shortest_unique_prefix([ "bearcat", "bert" ]) == ['bea', 'ber']
