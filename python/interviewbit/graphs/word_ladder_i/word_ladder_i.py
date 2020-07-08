'''
https://www.interviewbit.com/problems/word-ladder-i/

Word Ladder I

Given two words A and B, and a dictionary, C, find the length of shortest transformation sequence from A to B, such that:
	You must change exactly one character in every transformation.
	Each intermediate word must exist in the dictionary.

Note:
	Return 0 if there is no such transformation sequence.
	All words have the same length.
	All words contain only lowercase alphabetic characters.


Input Format:
	The first argument of input contains a string, A.
	The second argument of input contains a string, B.
	The third argument of input contains an array of strings, C.

Output Format:
Return an integer representing the minimum number of steps required to change string A to string B.

Constraints:
1 <= length(A), length(B), length(C[i]) <= 25
1 <= length(C) <= 5e3

Example :
Input 1:
    A = "hit"
    B = "cog"
    C = ["hot", "dot", "dog", "lot", "log"]

Output 1:
    5

Explanation 1:
    "hit" -> "hot" -> "dot" -> "dog" -> "cog"
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
class Solution:
	def find_min_transform_steps(self, A, B, C):
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

		n = len(A)
		from collections import defaultdict
		neighbors_lookup = defaultdict(list)
		add_to_lookup(A)
		add_to_lookup(B)
		for word in C:
			add_to_lookup(word)

		bfs_q = [(A,1)]
		visited = defaultdict(lambda: False)
		visited[A] = True
		while bfs_q:
			word, level = bfs_q.pop(0)
			for neighbor in get_neighbors(word):
				# Found 'end word'
				# return its current level
				# in the transformation path
				if neighbor == B:
					return (level+1)

				bfs_q.append((neighbor,level+1))
				visited[neighbor] = True

		# Couldn't find a transformation sequence from A to B
		return 0



if __name__ == '__main__':
	s = Solution()
	assert s.find_min_transform_steps("hit", "cog", ["hot", "dot", "dog", "lot", "log"]) == 5
	assert s.find_min_transform_steps("hit", "cog", ["hot", "dot", "pog", "lot", "rog"]) == 0

	assert s.find_min_transform_steps("red", "tax", ["ted","tex","red","tax","tad","den","rex","pee"]) == 4
	assert s.find_min_transform_steps("mate", "mile", ["math","path","male","mole","mile","late", "lake", "like"]) == 3
	assert s.find_min_transform_steps("ab", "bb", []) == 2

