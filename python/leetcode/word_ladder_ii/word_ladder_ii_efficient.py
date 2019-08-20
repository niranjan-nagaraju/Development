# -*- coding: utf-8 -*-

'''
https://leetcode.com/problems/word-ladder-ii/

* Problem
Given two words (beginWord and endWord), and a dictionary's word list, find all shortest transformation sequence(s) from beginWord to endWord, such that:

Only one letter can be changed at a time
Each transformed word must exist in the word list. Note that beginWord is not a transformed word.
Note:

Return an empty list if there is no such transformation sequence.
All words have the same length.
All words contain only lowercase alphabetic characters.
You may assume no duplicates in the word list.
You may assume beginWord and endWord are non-empty and are not the same.

Example 1:
Input:
beginWord = "hit",
endWord = "cog",
wordList = ["hot","dot","dog","lot","log","cog"]

Output:
[
  ["hit","hot","dot","dog","cog"],
  ["hit","hot","lot","log","cog"]
]

Example 2:
Input:
beginWord = "hit"
endWord = "cog"
wordList = ["hot","dot","dog","lot","log"]

Output: []
Explanation: The endWord "cog" is not in wordList, therefore no possible transformation.


* Solution description
  =====================
  + minlevel = ∞
  + Start with constructing a Directing Acyclic graph with Ws (= starting word) as starting vertex.
  + Add links from any word, Wj to word, Wi if Wj is one transformation away from Wi, in a breadth-first manner.
    - capture level, which is the distance of the current node representing Wx from Ws.
    - Add an edge from Wj to Wi only if Wi can be tranformed into Wj with a one-letter change.
      - Since we also capture level information for a vertex, a vertex can have many outgoing links to multiple parents
      - as long as the level remains intact.
	- this automatically prevents edges between two vertices in the same level.
           eg.
                  W1
               /      \
              w2  -   w3  (is not allowed) since w3 is also at level 1, adding edge from w3-w1 will make it level 2, and therefore not allowed)

        - also back edges w1-w2 and w2-w1 will be avoided, because w1-w2 puts w2 at level 1, w1 at level 0, w2-w1 will put w2 and w1 and different levels.

	- and more importantly, it allows 'diamond' pattern like below -

	     w1
       /    \
      w2     w3
       \     /
          w4

            Here w4 is firmly at level 2, but has two outgoing links to w2, and w3.

    - Since each outgoing edge from any vertex for Wi represents its parent, all the way to Ws,
      if we find We, Simply do a DFS to extract all paths from We to Ws.

  + Once We is found, and its level < minlevel, trace back all parent links until Ws.
    + Continue search for next We as long level == minlevel.


*** Example 1
Ws: "red"
We: "tax"
wordlist: ["ted","tex","red","tax","tad","den","rex","pee"]

minlevel = ∞

Level: 0
DAG: (red, 0)

Adjlist:
  [red]: (None, 0)

Level: 1
   (Add all words that are one letter away from red) -- [ted, rex]
   
   DAG:
         (red, 0)
        /       \
     (ted, 1)   (rex, 1)


                                       
Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)


Level 2:
(Add all words that are one letter away from ted) -- [tex, red, tad]
   red: (None, 0) cannot be replaced with red: (ted, 1)

   DAG:  
         (red, 0)
        /       \
     (ted, 1)   (rex, 1)
     /      \
   (tex,2) (tad,2) 

                                       
Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)
  [tex]: (ted, 2)
  [tad]: (ted, 2)
  
(Add all words that are one letter away from rex) -- [tex, red]
  red: level 0 != current level 2
  tex: level 2 == current level 2, can be added to DAG

   DAG:  
         (red, 0)
        /       \
     (ted, 1)   (rex, 1)
     /      \   /
   (tad,2) (tex,2) 

                                       
Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)
  [tex]: (ted, 2), (rex, 2)
  [tad]: (ted, 2)

Level 3:
(Add all words that are one letter away from tad) -- [ted, tax]
ted: level 1 != current level 3
tax: None

   DAG:  
         (red, 0)
        /       \
     (ted, 1)   (rex, 1)
     /      \   /
   (tad,2) (tex,2) 
  /
(tax,3)

Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)
  [tex]: (ted, 2), (rex, 2)
  [tad]: (ted, 2)
  [tax]: (tad, 3)*
     
minlevel = 3

(Add all words that are one letter away from tex) -- [ted, tax, rex]
ted: level 1 != current level 3
rex: level 1 != current level 3
tax: level 3 == current level 3, can be added to DAG

   DAG:  
         (red,0)
        /       \
     (ted,1)   (rex,1)
     /      \   /
   (tad,2) (tex,2) 
      /    /        
     (tax,3)

Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)
  [tex]: (ted, 2), (rex, 2)
  [tad]: (ted, 2)
  [tax]: (tad, 3), (tex, 3)

Level 4:
  > minlevel, Proceed to extract all paths from 'tax' to 'red' using DFS

paths:
  {tax, tad, ted, red}
  {tax, tex, ted, red}
  {tax, tex, rex, red}
'''

from collections import defaultdict

class Solution(object):
	# Initialize a lookup-table to quickly get one-letter transforms of a given word
	def initialize_lookup_table(self, wordList):
		lookup_table = defaultdict(set)
		for word in wordList:
			for i in xrange(len(word)):
				# Create different placeholder keys for lookup
				key = word[:i] + '*' + word[i+1:]

				# Using a set so incase there are duplicates in wordlist
				# (problem statement says there arent)
				# OR
				# beginWord already exists in wordList
				# in which case, we dont want duplicate entries in the lookup table
				lookup_table[key].add(word)

		return lookup_table


	# Find all words in the wordList that are one letter
	# away from 'word'
	def findOneLetterTranforms(self, word, lookup_table):
		transforms = []
		for i in xrange(len(word)):
			key = word[:i] + '*' + word[i+1:]
			words = lookup_table[key]
			for w in words:
				if w != word:
					transforms.append(w)

		return transforms


	# print the adjacency list representation of the graph
	def printGraph(self, graph):
		print 'Graph:'
		for j in graph.keys():
			print '[', j, ']:',
			if not graph[j]:
				print 'None'
			else:
				for w,level in graph[j]:
					print  (w, level),
				print

	# Extract all paths from endWord to beginWord, in reverse order
	# and return a list of paths
	def all_paths(self, graph, endWord, beginWord):
		# Use dfs to extract all paths
		# 'NOTE: keeping track of visited' is not needed as this is a DAG
		def dfs_paths(prefix, current):
			if current == beginWord:
				paths.append([beginWord]+prefix)

			for w,_ in graph[current]:
				dfs_paths([current]+prefix, w)



		paths = []
		dfs_paths([], endWord)
		return paths


	# Find minimumm-length ladders from beginWord -> endWord
	def findLadders(self, beginWord, endWord, wordList):
		"""
		:type beginWord: str
		:type endWord: str
		:type wordList: List[str]
		:rtype: List[List[str]]
		"""
		# Add beginWord to the list, just in case
		# wordList doesn't have it already
		wordList.insert(0, beginWord)

		lookup_table = self.initialize_lookup_table(wordList)


		shortest_paths = []

		bfs_q = [(beginWord, 0)] # Each 'vertex' in the graph has (parent word index, Level in the graph)

		# Maximum depth == max number of edges in the graph
		# this'll be equal to the number of words in the dictionary (sans Ws)
		# in the worst case
		minlength = len(wordList)

		# adjacency list representation of the transformation DAG
		# Edge Wj -> Wi exists if Wi can be transformed into Wj by changing one letter in Wi
		# Use a set for adjacency list so duplicate words wont be added
		transformation_graph = defaultdict(set)
		transformation_graph[beginWord].add((None, 0))
		while bfs_q:
			(word, curr_level) = bfs_q.pop(0)

			oneLetterTranforms= self.findOneLetterTranforms(word, lookup_table)

			curr_level += 1

			# We have found all the shortest transformation paths
			if curr_level > minlength:
				break

			for w in oneLetterTranforms:
				# add an edge to the parent word from current word,
				# capturing the level the current word is at
				# Add an edge only if it doesnt change the vertex's current level
				vertex_level = curr_level
				if transformation_graph[w]:
					# compare against an item in the adjacency list for w
					_,vertex_level = next(iter(transformation_graph[w]))

				if vertex_level == curr_level:
					transformation_graph[w].add( (word, curr_level) )

				# Found minimum ladder length
				# all transformations over this length needn't be included
				if w == endWord:
					minlength = curr_level
				else:
					# Do not add endWord to the BFS queue
					# its redundant to find 'neighbors' of endWord
					bfs_q.append((w, curr_level))


		#self.printGraph(transformation_graph)
		shortest_paths = self.all_paths(transformation_graph, endWord, beginWord)

		return shortest_paths


if __name__ == '__main__':
	s = Solution()

	ltable = s.initialize_lookup_table(["mate", "meat", "math", "late", "male", "mile"])

	assert sorted(s.findOneLetterTranforms("mate", ltable)) == sorted(["math", "late", "male"])
	assert sorted(s.findOneLetterTranforms("male", ltable)) == sorted(["mate", "mile"])

	assert s.findLadders("red", "tax", ["ted","tex","red","tax","tad","den","rex","pee"]) == [
			['red', 'ted', 'tad', 'tax'], ['red', 'ted', 'tex', 'tax'], ['red', 'rex', 'tex', 'tax']]

	assert s.findLadders("hit", "cog", ["hot","dot","dog","lot","log"]) == []
	assert s.findLadders("hit", "cog", ["hot","dot","dog","lot","log","cog"]) == [['hit', 'hot', 'lot', 'log', 'cog'], ['hit', 'hot', 'dot', 'dog', 'cog']]
	assert s.findLadders("mate", "mile", ["math","path","male","mole","mile","late", "lake", "like"]) == [['mate', 'male', 'mile']]
