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
  + Start with constructing a Tree/Directing Acyclic graph with Ws (= starting word) as starting vertex.
  + Add links from any word, Wj to word, Wi if Wj is one transformation away from Wi, in a breadth-first manner.
    - capture level, which is the distance of the current node representing Wx from Ws.
    - Add each word except We (= end word) exactly once into the DAG.
    - Since each outgoing edge from any vertex for Wi represents its parent, all the way to Ws,
      if we find We, simply retrace the path all the way back to Ws to get all the words involved in the transformation.
  + Once We is found, and its level < minlevel, trace back all parent links until Ws.
    + Continue search for next We as long level == minlevel.


*** EXAMPLE 1
    =========
ws = MATE
Dictionary = {MATH, PATH, MALE, MOLE, MILE, LATE, LAKE, LIKE}
We = MILE

minlevel = ∞
Level: 0
  Tree:   (MATE, 0)
  Adjlist:
  [MATE]:
   
Level: 1
   (Add all words that are one letter away from MATE)
   Tree:
            (MATE, 0) 
        /       |         \        
	(MATH, 1)  (MALE, 1)  (LATE, 1)

   Adjlist:
   [MATE]: 
   [MATH]: (MATE, 1)
   [MALE]: (MATE, 1)
   [LATE]: (MATE, 1)                

Level: 2
   (Add all words that are one letter away from MATH, but not already in the tree)
   Tree:
            (MATE, 0) 
        /       |         \        
	(MATH, 1)  (MALE, 1)  (LATE, 1)
	/
(PATH, 2)

   Adjlist:
   [MATE]: 
   [MATH]: (MATE, 1)
   [MALE]: (MATE, 1)
   [LATE]: (MATE, 1)
   [PATH]: (MATH, 2)

   (Add all words that are one letter away from MALE, but not already in the tree)
   Tree:
            (MATE, 0) 
        /       |         \        
	(MATH, 1)  (MALE, 1)  (LATE, 1)
	/            |      \  
(PATH, 2)      (MOLE, 2) (MILE, 2) == We (minlevel = 2)  [PATH = {MILE, MALE, MATE}]


   Adjlist:
   [MATE]: 
   [MATH]: (MATE, 1)
   [MALE]: (MATE, 1)
   [LATE]: (MATE, 1)
   [PATH]: (MATH, 2)
   [MOLE]: (MALE, 2)
   [MILE]: (MALE, 2)

   PATH: {MILE, MALE, MATE}

   (Add all words that are one letter away from LATE, but not already in the tree)
   Tree:
             (MATE, 0) 
        /       |         \        
	(MATH, 1)  (MALE, 1)  (LATE, 1) 
	/            |      \           \
(PATH, 2)   (MOLE, 2) (MILE, 2)     (LAKE, 2)


   Adjlist:
   [MATE]: 
   [MATH]: (MATE, 1)
   [MALE]: (MATE, 1)
   [LATE]: (MATE, 1)
   [PATH]: (MATH, 2)
   [MOLE]: (MALE, 2)
   [MILE]: (MALE, 2)
   [LAKE]: (LATE, 2)

Level 3: > minlevel, we are done here.



*** EXAMPLE 2:
    =========

Ws = HIT
Dictionary = {HOT, DOT, DOG, LOT, LOG, COG}
we = COG

minlevel = INFINITY
Level: 0
  Tree:   (HIT, 0)


Level: 1
   (Add all words that are one letter away from HIT)
   Tree:  
               (HIT, 0)
			/   
    (HOT, 1)

   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
		   
Level: 2
   (Add all words that are one letter away from HOT)
               (HIT, 0)
			/   
    (HOT, 1)
    /	   \
(DOT, 2)  (LOT, 2)

   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
   [DOT]: (HOT, 2)
   [LOT]: (HOT, 2)

Level: 3
   (Add all words that are one letter away from DOT)
               (HIT, 0)
    			/   
          (HOT, 1)
           /	   \
        (DOT, 2)  (LOT, 2)
         /
	  (DOG, 3)	 

   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
   [DOT]: (HOT, 2)
   [LOT]: (HOT, 2)
   [DOG]: (DOT, 3)

   (Add all words that are one letter away from LOT)
              (HIT, 0)
    			/   
          (HOT, 1)
           /	   \
        (DOT, 2)  (LOT, 2)
         /         /
	  (DOG, 3)	 (LOG, 3)


   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
   [DOT]: (HOT, 2)
   [LOT]: (HOT, 2)
   [DOG]: (DOT, 3)
   [LOG]: (LOT, 3)

Level: 4
   (Add all words that are one letter away from DOG)
            (HIT, 0)
    			/   
          (HOT, 1)
           /	   \
        (DOT, 2)  (LOT, 2)
         /         /
	  (DOG, 3)	 (LOG, 3)
       /
     (COG, 4)		

   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
   [DOT]: (HOT, 2)
   [LOT]: (HOT, 2)
   [DOG]: (DOT, 3)
   [LOG]: (LOT, 3)
   [COG]: (DOG, 4) == We (minlevel = 4)

 *Path: {COG, DOG, DOT, HOT, HIT}*

   (Add all words that are one letter away from LOG)
            (HIT, 0)
    			/   
          (HOT, 1)
           /	   \
        (DOT, 2)  (LOT, 2)
         /         /
	  (DOG, 3)	 (LOG, 3)
       /           /
     (COG, 4)    (COG, 4)		

   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
   [DOT]: (HOT, 2)
   [LOT]: (HOT, 2)
   [DOG]: (DOT, 3)
   [LOG]: (LOT, 3)
   [COG]: (LOG, 4) == We (minlevel = 4)
   
   Path: 
      {COG, DOG, DOT, HOT, HIT}, 
      {COG, LOG, LOT, HOT, HIT}

Level: 5
   > minlevel, exit
   (Add all words that are one letter away from COG)



*** EXAMPLE 3:  (DOES NOT RETURN ALL SHORTEST-PATHS)
    =========
Ws = red
Dictionary = [ted, tex, red, tax, tad, den, rex, pee]
we = tax

minlevel = INFINITY
Level: 0
  Tree:   (red, 0)


Level: 1
   (Add all words that are one letter away from red) -- [ted, rex]
   
   Tree:  
         (red, 0)
          /    \
    (ted, 1)   (rex, 1)

                                    
Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)

Level: 2
(Add all words that are one letter away from ted) -- [tad, tex]
   Tree:  
         (red, 0)
          /        \
     (ted, 1)      (rex, 1)
      /      \
    (tad, 2) (tex, 2)

Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)
  [tad]: (ted, 2)
  [tex]: (ted, 2)

(Add all words that are one letter away from rex) -- []
   Tree:  
         (red, 0)
          /        \
     (ted, 1)      (rex, 1)
      /      \
    (tad, 2) (tex, 2)

Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)
  [tad]: (ted, 2)
  [tex]: (ted, 2)

Level 3:
(Add all words that are one letter away from tad) -- [tax]
   Tree:  
         (red, 0)
          /        \
     (ted, 1)      (rex, 1)
      /      \
    (tad, 2) (tex, 2)
    /
  (tax, 3)*

Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)
  [tad]: (ted, 2)
  [tex]: (rex, 2)
  [tax]: (tad, 3)   -> path: {tax, tad, ted, red}

(Add all words that are one letter away from tex) -- [tax]
   Tree:  
         (red, 0)
          /        \
     (ted, 1)      (rex, 1)
      /      \
    (tad, 2) (tex, 2)
    /          /
  (tax, 3)*   (tax, 3)*


Adjlist: 
  [red]: (None, 0)
  [ted]: (red, 1)
  [rex]: (red, 1)
  [tad]: (ted, 2)
  [tex]: (ted, 2)
  [tax]: (tex, 3)   -> path: {tax, tad, ted, red}, {tax, tex, ted, red}

Output: [[tax, tad, ted, red], [tax, tex, ted, red]]
Expected output: [[red, ted, tad, tax], [red, ted, tex, tax], [red, rex, tex, tax]]
Missing: [red, rex, tex, tax]
because it forms a diamond pattern, so certainly not a tree, and a vertex _can_ have multiple parents.
so, a proper DAG is needed.
However, if only one shortest-paths or just a subset suffices, then this can be used as a viable alternative.


    red
  /     \
rex    ted
  \    /  \
   tex    tad
     |     |
    tax   tax 



'''


class Solution(object):
	# Check whether if word1, and word2 are one letter away from each other
	def isTransformation(self, word1, word2):
		num_changes = 0
		for i in xrange(len(word1)):
			if word1[i] != word2[i]:
				num_changes += 1

			if num_changes > 1:
				return False
		return True if num_changes == 1 else False


	# Find all words in the wordList that are one letter
	# away from 'word'
	def findOneLetterTranforms(self, word, wordList, visited):
		transforms = []
		for i in xrange(len(wordList)):
			if visited[i] is False and self.isTransformation(word, wordList[i]):
				transforms.append(i)

		return transforms


	# Retrace path to Ws from We and add them to the list of paths
	def add_to_path(self, paths, startIdx, endIdx, wordList, graph):
		path = []
		i = endIdx
		while i != None:
			path.insert(0, wordList[i])
			i,_ = graph[i]

		if path not in paths:
			paths.append(path)


	# print the adjacency list representation of the graph
	def printGraph(self, graph, wordList, visited):
		print 'Graph:'
		for j in xrange(len(graph)):
			print '[', wordList[j], ']:', (wordList[graph[j][0]], graph[j][1]) if graph[j] else None,
			print 'Visited:', visited[j]


	# Find minimumm-length ladders from beginWord -> endWord
	def findLadders(self, beginWord, endWord, wordList):
		"""
		:type beginWord: str
		:type endWord: str
		:type wordList: List[str]
		:rtype: List[List[str]]
		"""
		startIdx = None
		for i in xrange(len(wordList)):
			if wordList[i] == beginWord:
				startIdx = i
				break

		# add beginWord to wordList if it doesn't exist in the wordlist
		# so each word becomes a vertex in the graph
		if startIdx is None:
			wordList.insert(0, beginWord)
			startIdx = 0

		endIdx = None
		for i in xrange(len(wordList)):
			if wordList[i] == endWord:
				endIdx = i
				break

		if endIdx is None:
			return []

		shortest_paths = []
		visited = [False] * len(wordList)

		bfs_q = [(startIdx, 0)] # Each 'vertex' in the graph has (parent word index, Level in the graph)
		visited[startIdx] = True  # Mark startword as visited

		# Maximum depth == max number of edges in the graph
		# this'll be equal to the number of words in the dictionary (sans Ws)
		# in the worst case
		minlength = len(wordList)

		# adjacency list representation of the transformation DAG
		# Edge Wj -> Wi exists of Wi can be transformed into Wj by changing one letter in Wi
		# Since each vertex has exactly one parent, this can just be a simple 1-D list
		# instead of list of lists as adjacency lists usually are.
		transformation_graph = [None for x in xrange(len(wordList))]
		transformation_graph[startIdx] = (None, 0)
		while bfs_q:
			(wordIdx, curr_level) = bfs_q.pop(0)

			oneLetterTranformsIdxs = self.findOneLetterTranforms(wordList[wordIdx], wordList, visited)

			curr_level += 1

			# We have found all the shortest transformation paths
			if curr_level > minlength:
				break

			for i in oneLetterTranformsIdxs:
				# add an edge to the parent word from current word,
				# capturing the level the current word is at
				transformation_graph[i] = (wordIdx, curr_level)
				#self.printGraph(transformation_graph, wordList, visited)

				if i == endIdx:
					minlength = curr_level
					self.add_to_path(shortest_paths, startIdx, endIdx, wordList, transformation_graph)
				else:
					# Do not mark endWord as visited ever.
					visited[i] = True
					bfs_q.append((i, curr_level))


		#self.printGraph(transformation_graph, wordList, visited)
		return shortest_paths

if __name__ == '__main__':
	s = Solution()


	assert s.isTransformation("mate", "mate") == False
	assert s.isTransformation("mate", "math") == True
	assert s.isTransformation("mate", "meat") == False

	assert s.findOneLetterTranforms("mate", ["mate", "meat", "math", "late", "male", "mile"], [False]*6) == [2, 3, 4]

	assert s.findLadders("hit", "cog", ["hot","dot","dog","lot","log"]) == []
	assert s.findLadders("hit", "cog", ["hot","dot","dog","lot","log","cog"]) == [['hit', 'hot', 'dot', 'dog', 'cog'], ['hit', 'hot', 'lot', 'log', 'cog']]
	assert s.findLadders("mate", "mile", ["math","path","male","mole","mile","late", "lake", "like"]) == [['mate', 'male', 'mile']]


	'''
	Expected:
	[["red","ted","tad","tax"],["red","ted","tex","tax"],["red","rex","tex","tax"]]

	Algorithm returns: 2 out of 3 shortest-paths because the shortest-paths *can* form a DAG, and algorithm constructs a tree not a DAG
	[["red","ted","tex","tax"],["red","ted","tad","tax"]]
	'''
	assert s.findLadders("red", "tax", ["ted","tex","red","tax","tad","den","rex","pee"]) == [['red', 'ted', 'tex', 'tax'], ['red', 'ted', 'tad', 'tax']]


