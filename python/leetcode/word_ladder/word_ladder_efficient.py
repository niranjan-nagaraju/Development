# -*- coding: utf-8 -*-

'''
https://leetcode.com/problems/word-ladder

* Problem
Given two words (beginWord and endWord), and a dictionary's word list, find the length of shortest transformation sequence from beginWord to endWord, such that:

Only one letter can be changed at a time.
Each transformed word must exist in the word list. Note that beginWord is not a transformed word.
Note:

Return 0 if there is no such transformation sequence.
All words have the same length.
All words contain only lowercase alphabetic characters.
You may assume no duplicates in the word list.
You may assume beginWord and endWord are non-empty and are not the same.

Example 1:
Input:
beginWord = "hit",
endWord = "cog",
wordList = ["hot","dot","dog","lot","log","cog"]
Output: 5
Explanation: As one shortest transformation is "hit" -> "hot" -> "dot" -> "dog" -> "cog",
return its length 5.

Example 2:
Input:
beginWord = "hit"
endWord = "cog"
wordList = ["hot","dot","dog","lot","log"]
Output: 0
Explanation: The endWord "cog" is not in wordList, therefore no possible transformation.
'''

'''
Solution Outline:
	1. Build a graph starting from 'A', the starting word, s.t the word at each level is one-letter different from the previous level.
	2. Do a BFS traversal from 'A', the starting word until we reach 'B' the end word.
		Calculate the path length - this will be the minimum number of steps needed to change from A to B.

	To build the graph,
	  Build a lookup table, of words A, B, and each word in C, and their neighbors.
	  neighbors of word, w: words that are one-letter distance away from w
	  Let word w: {w1,w2,..wn}
		  Breakup word into 'n' parts, like below, and add 'word' into each of these 'n' parts as keys.
		  {_, w2, .., wn}: word
		  {w1, _, .., wn}: word
		  .  .  .  .  .
		  {w1, w2, .., _}: word

	  After processing all words,
	  Then, for each word, w,
		its 'neighbors' are the combination of values at keys:
			{_, w2, .., wn}
			{w1, _, .., wn}
			.  .  .  .  .
			{w1, w2, .., _}

	For.e,g, A: "hit", B: "cog", C: ["hot", "dot", "dog", "lot", "log"]
	  "hit"
		"_it": ["hit"]
		"h_t": ["hit"]
		"hi_": ["hit"]
	  "hot":
		"_ot": ["hot"]
		"h_t": ["hit", "hot"]
		"ho_": ["hot"]
	  "dot"
		"_ot": ["hot", "dot"]
		"d_t": ["dot"]
		"do_": ["dot"]
	  "dog":
		"_og": ["dog"]
		"d_g": ["dog"]
		"do_": ["dot", "dog"]
	  "lot"
		"_ot": ["hot", "dot", "lot"]
		"l_t": ["lot"]
		"lo_": ["lot"]
	  "log"
		"_og": ["dog", "log"]
		"l_g": ["log"]
		"lo_": ["lot", "log"]
	  "cog"
		"_og": ["dog", "log", "cog"]
		"c_g": ["cog"]
		"co_": ["cog"]

	Now use the lookup table as the graph, to get neighbors of each word, and perform a BFS, starting from 'A',
	until we reach 'B', while recording the number of levels

	Q: [("hit",0)]
	visited: ["hit"]

	Dequeue: ("hit", 0)
	Q: []
	neighbor of "hit": lookup["_it"] + lookup["h_t"] + lookup["hi_"]
					 : ["hit"] + ["hit", "hot"] + ["hit"]   {-remove "hit"}
					 : ["hot"]

    hit
      \ 
        - - hot
	Q: [("hot",1)]
	visited: ["hit", "hot"]

	Dequeue: ("hot", 1)
	Q: []
	neighbor of "hot": lookup["_ot"] + lookup["h_t"] + lookup["ho_"]
					 : ["hot", "dot", "lot"] + ["hit","hot"] + ["hot"] {-remove "hot" and "hit" (already visited)}
					 : ["dot", "lot"]
    hit
      \ 
        - - hot
              \    / - dot
                - -
                   \ - lot

	Q: [("dot",2), ("lot",2)]
	visited: ["hit", "hot", "dot", "lot"]

	Dequeue: ("dot", 2)
	Q: [("lot",2)]
	neighbor of "dot": lookup["_ot"] + lookup["d_t"] + lookup["do_"]
					 : ["hot", "dot", "lot"] + ["dot"] + ["dot", "dog"] {-remove "hot", "hit", "lot" and "dot" (already visited)}
					 : ["dog"]
	Q: [("lot",2), ("dog",3)]
	visited: ["hit", "hot", "dot", "lot", "dog"]

	Dequeue: ("lot",2)
	neighbor of "lot": lookup["_ot"] + lookup["l_t"] + lookup["lo_"]
					 : ["hot", "dot", "lot"] + ["lot"] + ["lot", "log"] {remove visited}
					 : ["log"]
	Q: [("dog",3), ("log",3)]
	visited: ["hit", "hot", "dot", "lot", "dog", "log"]
	
	Dequeue: ("dog", 3)
	Q: [("log",3)]
    hit
      \ 
        - - hot
              \    / - dot -- dog
                - -
                   \ - lot

	neighbor of "dog": lookup["_og"] + lookup["d_g"] + lookup["do_"]
					 : ["dog", "log", "cog"] + ["dog"] + ["dot", "dog"] {remove visited}
					 : ["cog"]
	Q: [("log",3), ("cog",4)]
	visited: ["hit", "hot", "dot", "lot", "dog", "log", "cog"]
	Found 'cog' at level 4
	hit
      \ 
        - - hot
              \    / - dot -- dog -- cog
                - -
                   \ - lot

	return (4+1) == 5
'''

class Solution(object):
	def ladderLength(self, beginWord, endWord, wordList):
		"""
		:type beginWord: str
		:type endWord: str
		:type wordList: List[str]
		:rtype: int
		"""
		# Build a lookup table of neighbors
		# for current word
		def add_to_lookup(word):
			for i in xrange(n):
				key = word[:i] + '_' + word[i+1:]
				neighbors_lookup[key].append(word)

		# Get neighbors that are 1-letter distance away for a specified word
		def get_neighbors(word):
			neighbors = []
			for i in xrange(n):
				key = word[:i] + '_' + word[i+1:]
				for nw in neighbors_lookup[key]:
					if not visited[nw]:
						neighbors.append(nw)

			return neighbors

		n = len(beginWord)
		from collections import defaultdict
		neighbors_lookup = defaultdict(list)

		# Add only beginWord, and wordList words to the lookup table
		# so if the endWord is not in the dictionary,
		# BFS will fail to locate it
		add_to_lookup(beginWord)
		for word in wordList:
			add_to_lookup(word)

		bfs_q = [(beginWord,1)]
		visited = defaultdict(lambda: False)
		visited[beginWord] = True
		while bfs_q:
			word, level = bfs_q.pop(0)
			for neighbor in get_neighbors(word):
				# Found 'end word'
				# return its current level
				# in the transformation path
				if neighbor == endWord:
					return (level+1)

				bfs_q.append((neighbor,level+1))
				visited[neighbor] = True

		# Couldn't find a transformation sequence from beginWord to endWord
		return 0


if __name__ == '__main__':
	s = Solution()
	assert s.ladderLength("hit", "cog", ["hot","dot","dog","lot","log"]) == 0
	assert s.ladderLength("hit", "cog", ["hot","dot","dog","lot","log","cog"]) == 5
	assert s.ladderLength("red", "tax", ["ted","tex","red","tax","tad","den","rex","pee"]) == 4
	assert s.ladderLength("mate", "mile", ["math","path","male","mole","mile","late", "lake", "like"]) == 3

	from test_in import we, ws, wl
	assert s.ladderLength(ws, we, wl) == 20

