'''
https://www.interviewbit.com/problems/word-ladder-ii/

Word Ladder II

Given two words (start and end), and a dictionary, find the shortest transformation sequence from start to end, such that:
	Only one letter can be changed at a time
	Each intermediate word must exist in the dictionary

If there are multiple such sequence of shortest length, return all of them. Refer to the example for more details.

Note:
	All words have the same length.
	All words contain only lowercase alphabetic characters.

Input Format
	The first argument is string start.
	The second argument is string end.
	The third argument is an array of strings dict

Output Format
Return all transformation sequences such that first word of each sequence is start and last word is end, all intermediate words belongs to dictionary(dict) and consecutive words had atmost 1 difference. 

Example :
	start = "hit"
	end = "cog"
	dict = ["hot","dot","dog","lot","log"]
Return
  [
    ["hit","hot","dot","dog","cog"],
    ["hit","hot","lot","log","cog"]
  ]
'''

'''
Solution Outline:
	0. Let A: starting word, B: ending word, C: wordlist/dictionary
	1. Build a graph starting from 'A', the starting word, s.t the word at each level is one-letter different from the previous level.
	2. Do a BFS traversal from 'A', the starting word until we reach 'B' the end word, recording 'prefix' of the current path along the way.
		To find all the minimum-length transformation paths from A to B,
		Re-purpose visited[] table as a levels+visited lookup.
		visited[word] =  level => indicates 'word' was visited at 'level' steps away from starting word.
		Add 'neighbor' to the BFS queue only if its not already visited or its level is going to be the same.
		This ensures, 
			i.  We don't add paths that exceed minimum-length
			ii. All the minimum-length paths are added

		for e.g., Consider the graph below, S: starting word, E: ending word
           S
        /  |  \       
       a   b   c
        \  |  /
           d
           |
           E

		we need 'd' to be added thrice for [S, a, d, E], [S, b, d, E], [S, c, d, E]
	3. Once we find the endWord using BFS for the first time, its recorded visited[] level stores the minimum steps needed to get to end word from start.
		We can return as soon as this level is processed.

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

	Q: [("hit", [])]
	visited: 
		{"hit": 1}

	Dequeue: ("hit", [])
	Q: []
	neighbor of "hit": lookup["_it"] + lookup["h_t"] + lookup["hi_"]
					 : ["hit"] + ["hit", "hot"] + ["hit"]   {remove visited}
					 : ["hot"]

    hit
      \ 
        - - hot
	Q: [("hot", {"hit"})]
	visited: 
		{
		 "hit": 1, 
		 "hot": 2
		}

	Dequeue: ("hot", {"hit"})
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

	Q: [("dot", {"hit", "hot"}), ("lot",{"hit", "hot"})]
	visited: 
		{
		 "hit": 1,
		 "hot": 2,
		 "dot": 3,
		 "lot": 3
		}

	Dequeue: ("dot", {"hit", "hot"})
	Q: [("lot", {"hit", "hot"})]
	neighbor of "dot": lookup["_ot"] + lookup["d_t"] + lookup["do_"]
					 : ["hot", "dot", "lot"] + ["dot"] + ["dot", "dog"] {-remove "hot", "hit", "lot" and "dot" (already visited)}
					 : ["dog"]
	Q: [("lot", {"hit", "hot"}), ("dog", {"hit", "hot", "dot"})]
	visited: 
		{
		 "hit": 1,
		 "hot": 2,
		 "dot": 3,
		 "lot": 3,
		 "dog": 4
		}


	Dequeue: ("lot", {"hit", "hot"})
	neighbor of "lot": lookup["_ot"] + lookup["l_t"] + lookup["lo_"]
					 : ["hot", "dot", "lot"] + ["lot"] + ["lot", "log"] {remove visited}
					 : ["log"]
	Q: [("dog", {"hit", "hot", "dot"}), ("log", {"hit", "hot", "lot"})]
	visited: 
		{
		 "hit": 1,
		 "hot": 2,
		 "dot": 3,
		 "lot": 3,
		 "dog": 4,
		 "log": 4
		}
	
	Dequeue: ("dog", {"hit", "hot", "dot"})
	Q: [("log", {"hit", "hot", "lot"})]
    hit
      \ 
        - - hot
              \    / - dot -- dog
                - -
                   \ - lot

	neighbor of "dog": lookup["_og"] + lookup["d_g"] + lookup["do_"]
					 : ["dog", "log", "cog"] + ["dog"] + ["dot", "dog"] {remove visited}
					 : ["cog"]
	Q: [("log", {"hit", "hot", "lot"}), ("cog", {"hit", "hot", "dot", "dog"})]
	visited: 
		{
		 "hit": 1,
		 "hot": 2,
		 "dot": 3,
		 "lot": 3,
		 "dog": 4,
		 "log": 4,
		 "cog": 5
		}
	
	Found 'cog' at level 5
	hit
      \ 
        - - hot
              \    / - dot -- dog -- cog
                - -
                   \ - lot

	Minimum level ==  (4+1) == 5
	=> None of our paths should exceed 5 words from "hit" to "cog"

	Dequeue: ("log", {"hit", "hot", "lot"})
	Q: [("cog", {"hit", "hot", "dot", "dog"})]
    hit
      \ 
        - - hot
              \    / - dot -- dog
                - -
                   \ - lot -- log

	neighbor of "log": lookup["_og"] + lookup["l_g"] + lookup["lo_"]
					 : ["dog", "log", "cog"] + ["log"] + ["lot", "log"] {remove visited}
					 : ["cog"]

	"cog" is already visited at level 5, this enqueue will also put it at level 5
	Add "cog" to queue
	Q: [("cog", {"hit", "hot", "dot", "dog"}), ("cog", {"hit", "hot", "lot", "log"})]
	visited: 
		{
		 "hit": 1,
		 "hot": 2,
		 "dot": 3,
		 "lot": 3,
		 "dog": 4,
		 "log": 4,
		 "cog": 5
		}

	Dequeue: ("cog", {"hit", "hot", "dot", "dog"})
	found endword
		Add to solution: {"hit", "hot", "dot", "dog", "cog"}
    hit
      \ 
        - - hot
              \    / - dot -- dog -- cog
                - -
                   \ - lot -- log


	Dequeue: ("cog", {"hit", "hot", "lot", "log"})
	found endword
		Add to solution: {"hit", "hot", "dot", "dog", "cog"}
						 {"hit", "hot", "lot", "log", "cog"}
    hit
      \ 
        - - hot
              \    / - dot -- dog -- \
                - -                  cog
                   \ - lot -- log -- /


  At this point if there were additional levels in the queue, we return as even if they lead to 'cog', they exceed minimum length.
'''

from collections import defaultdict
class Solution(object):
	# Find minimum-length ladders from beginWord -> endWord
	def findLadders(self, beginWord, endWord, wordList):
		"""
		:type beginWord: str
		:type endWord: str
		:type wordList: List[str]
		:rtype: List of transformation paths
		"""
		# Build a lookup table of neighbors
		# for current word
		def add_to_lookup(word):
			for i in xrange(n):
				key = word[:i] + '_' + word[i+1:]
				neighbors_lookup[key].append(word)

		if beginWord == endWord:
			return [[beginWord]]

		n = len(beginWord)
		neighbors_lookup = defaultdict(list)

		# Add beginWord, endWord, and wordList words to the lookup table
		# WTF: wordList seems to contain beginWord and endWord!!!
		#add_to_lookup(beginWord)
		#add_to_lookup(endWord)
		for word in set(wordList):
			# WTF: wordList can contain duplicates !!!
			add_to_lookup(word)

		# Use BFS to find transformations
		# needed to transform startWord to endWord
		bfs_q = [(beginWord,[])]
		visited = defaultdict(lambda: 0, {beginWord: 1})
		paths = []
		while bfs_q:
			word, prefix = bfs_q.pop(0)
			if len(prefix) == visited[endWord]-1:
				# prefix is already the same as endWord's level
				# even if this path leads to endWord, it will exceed the minimum steps
				# seen so far
				# Return all the paths recorded so far
				return paths

			for i in xrange(n):
				key = word[:i] + '_' + word[i+1:]
				for neighbor in neighbors_lookup[key]:
					if neighbor == word:
						continue

					if neighbor == endWord:
						# Found 'end word'
						# return its current level
						# in the transformation path
						# NOTE: As a slight optimization, do not add 'endWord' to the queue
						# we already know its path, and we dont intend to traverse beyond it
						paths.append(prefix + [word] + [neighbor])
						visited[endWord] = visited[word]+1
					else:
						if not visited.get(neighbor) or visited[neighbor] == (visited[word]+1):
							# current word is either not visited yet
							# or it will remain at the same level for a different path
							bfs_q.append((neighbor,prefix+[word]))
							visited[neighbor] = visited[word]+1
		return paths




if __name__ == '__main__':
	s = Solution()
	assert s.findLadders("red", "tax", ["ted","tex","red","tad","den","rex","pee", "tax"]) == [
			['red', 'ted', 'tad', 'tax'], ['red', 'ted', 'tex', 'tax'], ['red', 'rex', 'tex', 'tax']]
	assert sorted(s.findLadders("hit", "cog", ["hit", "hot","dot","dog","lot", "log", "cog"])) == \
			sorted([['hit', 'hot', 'lot', 'log', 'cog'], ['hit', 'hot', 'dot', 'dog', 'cog']])
	assert s.findLadders("mate", "mile", ["mate", "mile", "math","path","male","mole","late", "lake", "like"]) == [['mate', 'male', 'mile']]
	assert s.findLadders("bb", "bb", ["bb", "bb"]) == [["bb"]]
	assert s.findLadders("ab", "bb", ["ab", "bb"]) == [["ab","bb"]]

