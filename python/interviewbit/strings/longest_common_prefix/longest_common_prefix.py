'''
https://www.interviewbit.com/problems/longest-common-prefix/

Longest Common Prefix

Given the array of strings A,
you need to find the longest string S which is the prefix of ALL the strings in the array.

Longest common prefix for a pair of strings S1 and S2 is the longest string S which is the prefix of both S1
and S2.

For Example, longest common prefix of "abcdefgh" and "abcefgh" is "abc".

nput Format

The only argument given is an array of strings A.
Output Format

Return longest common prefix of all strings in A.
For Example

Input 1:
    A = ["abcdefgh", "aefghijk", "abcefgh"]
Output 1:
    "a"
Explanation 1:
	Longest common prefix of all the strings is "a".

Input 2:
    A = ["abab", "ab", "abcd"];
Output 2:
    "ab"
Explanation 2:
	Longest common prefix of all the strings is "ab".
'''

'''
Solution Outline:
	Observations: The longest common prefix only decreases(/or stays the same) with the introduction of each new word, never increases.
	E.g, 
	  w1: "abcde" , lcp initially is 5
	  w2: "abcd", lcp is now 4

	This is true regardless of the order in which the words arrive.
	  w1: "abcd" , lcp initially is 4
	  w2: "abcde", lcp stays at 4

	0. Use a Trie
	1. Add initial word, word0,  onto the trie.
	   Set LCP to len(word0)
	2. For each word after word0, "add" to the trie only until they diverge.
	   Update the length of the path until they diverge, as the new LCP.
	    for e.g,
		  w0: "abcd"
		  Trie:
            a
           /	
          b 	
         /
        c
       /
      d

      LCP = 4

	  w1: "abef"
	  Adding 'e' requires a new node, return 2 as the new LCP

	  w3: "abcf"
	  Add only first 2(=LCP) characters from w3 into the trie and see if they diverge.
	  They dont. => return 2 as LCP

	  w4: "axdfghjghkh"
	  Add only first 2(=LCP) characters from w4 into the trie and see if they diverge.
	  Adding 'x' requires a new child node from 'a'
	  => return 1 as the LCP

	3. Return the last updated LCP
'''
class Trie(object):
	class TrieNode(object):
		def __init__(self):
			self.children = {}

	# Initialize Trie with word
	def __init__(self, word):
		self.root = Trie.TrieNode()

		node = self.root
		for c in word:
			node.children[c] = Trie.TrieNode()
			node = node.children[c]


	# Add 'max_len' characters from 'word' onto the trie
	# if at any point, adding character, c, in word
	# needs a new node, stop adding and return this as the LCP
	# of all words so far
	def add(self, word, max_len):
		i = 0
		node = self.root
		while i < max_len and i<len(word):
			c = word[i]
			if not node.children.has_key(c):
				break

			node = node.children[c]
			i += 1

		return i

class Solution:
	def find_longest_common_prefix(self, A):
		if len(A) == 0 or len(A[0]) == 0:
			return 0

		trie = Trie(A[0])
		lcp = len(A[0])

		for i in xrange(1, len(A)):
			word = A[i]
			lcp = trie.add(word, lcp)

		return A[0][:lcp]



if __name__ == '__main__':
	t = Trie("abcd")
	assert t.root.children.keys() == ['a']
	assert t.root.children['a'].children.keys() == ['b']
	assert t.root.children['a'].children['b'].children.keys() == ['c']
	assert t.root.children['a'].children['b'].children['c'].children.keys() == ['d']

	assert t.add("abx", 4) == 2
	assert t.add("abcde", 2) == 2
	assert t.add("x", 2) == 0
	assert t.add("", 2) == 0

	s = Solution()
	assert s.find_longest_common_prefix(["abcd", "abce", "abxf"]) == "ab"
	assert s.find_longest_common_prefix(["abxf", "abcd", "abce"]) == "ab"
	assert s.find_longest_common_prefix(["abcd", "abce", "axf"]) == "a"
	assert s.find_longest_common_prefix(["abcd", "abce", "f"]) == ""
	assert s.find_longest_common_prefix(["abcd", "abce", ""]) == ""
	assert s.find_longest_common_prefix(["abcd", "abcde", ""]) == ""

