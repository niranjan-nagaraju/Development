#+encoding: utf-8

'''
https://www.hackerearth.com/problem/algorithm/unique-prefixes/

Problem
Suppose in a list of n distinct words, no word is a prefix of another word. In that case, each word has a unique prefix too, often shorter than word, which can be used to distinctly represent the word among the list of n words. Find out the shortest unique prefix of each word.

Input Format:
	Input starts with a positive integer n indicating the number of words to follow, one in each line. A word in each line n lines.

Output Format:
	Output to have n lines, one line for each corresponding input word, with the shortest prefix of the word.

Constraints:
	1 ≤ n ≤ 10^6
	1 ≤ length of word ≤ 10^3
	1 ≤ sum of the number of characters of all the words ≤ 10^6

Sample Input
	6
	pesu
	guru
	suffix
	prefix
	pesit
	science
Sample Output
	pesu
	g
	su
	pr
	pesi
	sc
'''

'''
Solution Outline:
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
	class Node(object):
		def __init__(self):
			self.children = {}
			self.eow = False
			self.words = [] # words that match this prefix

		# return a `child` node from current node
		# towards the path representing 'current node prefix + letter'
		def getChild(self, letter):
			if not self.children.has_key(letter):
				self.children[letter] = Trie.Node()
			return self.children[letter]

	def __init__(self):
		self.root = Trie.Node()

	def add(self, word, idx):
		node = self.root
		node.words.append(idx)
		for c in word:
			child = node.getChild(c)
			node = child
			node.words.append(idx)
		node.eow = True


class Solution:
	def shortest_unique_prefix(self, words):
		def dfs(node, prefix=""):
			if node is None:
				return

			children = node.children.keys()
			if len(node.words) == 1 or node.eow:
				sups[node.words[0]] = prefix
				return

			for n in children:
				dfs(node.getChild(n), prefix+n)


		if len(words) < 2:
			return words

		t = Trie()
		for idx,word in enumerate(words):
			t.add(word, idx)

		sups = [None] * len(words)
		dfs(t.root)

		return sups

def basic_testcases():
	t = Trie()
	t.add("abc", 0)
	assert(t.root.getChild('a').getChild('b').getChild('c').eow == True)
	assert(t.root.getChild('a').eow == False)
	assert(t.root.getChild('a').getChild('b').eow == False)
	t.add("acd", 1)
	assert(t.root.getChild('a').getChild('c').getChild('d').eow == True)

	root = t.root
	path_a = t.root.getChild('a')
	assert path_a.words == [0,1]
	assert sorted(path_a.children.keys()) == ['b', 'c']
	path_ac = t.root.getChild('a').getChild('c')
	assert path_ac.words == [1]
	assert path_ac.children.keys() == ['d']

	s = Solution()
	assert s.shortest_unique_prefix(['zebra', 'dog', 'duck', 'dove']) ==\
			['z', 'dog', 'du', 'dov']

	assert s.shortest_unique_prefix([ "zebra", "dog", "duck", "dot" ]) == \
			['z', 'dog', 'du', 'dot']
	assert s.shortest_unique_prefix([ "bearcat", "bert" ]) == ['bea', 'ber']
	assert s.shortest_unique_prefix(["pesu", "guru", "suffix", "prefix", "pesit", "science"]) == \
			["pesu", "g", "su", "pr", "pesi", "sc"]


if __name__ == '__main__':
	basic_testcases()
	n = int(input())
	words = [raw_input().strip() for _ in xrange(n)]
	s = Solution()
	for prefix in s.shortest_unique_prefix(words):
		print prefix

