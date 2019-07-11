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


* Solution description
  + minlevel = ∞
  + Start with constructing a Directing Acyclic graph with Ws (= starting word) as starting vertex.
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
  DAG:   (MATE, 0)
  Adjlist:
  [MATE]:
   
Level: 1
   (Add all words that are one letter away from MATE)
   DAG:
         (MATE, 0) 
                   | <- (MATH, 1)
                   | <- (MALE, 1)
                   | <- (LATE, 1)
   Adjlist:
   [MATE]: 
   [MATH]: (MATE, 1)
   [MALE]: (MATE, 1)
   [LATE]: (MATE, 1)                

Level: 2
   (Add all words that are one letter away from MATH, but not already in the tree)
   Tree:
         (MATE, 0, None)
                   |     <- (MATH, 1) 
		                      |     <- (PATH, 2)
                   |     <- (MALE, 1) 
                   |     <- (LATE, 1)                   

   Adjlist:
   [MATE]: 
   [MATH]: (MATE, 1)
   [MALE]: (MATE, 1)
   [LATE]: (MATE, 1)
   [PATH]: (MATH, 2)

   (Add all words that are one letter away from MALE, but not already in the tree)
   Tree:
         (MATE, 0, None) 
                   |     <- (MATH, 1) 
		                      |     <- (PATH, 2)
                   |     <- (MALE, 1) 
                                      |     <- (MOLE, 2)
		                      |     <- (MILE, 2)  == We (minlevel = 2)  [PATH = {MILE, MALE, MATE}]
                   |     <- (LATE, 1)                   

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
         (MATE, 0, None) 
                   |     <- (MATH, 1) 
		                      |     <- (PATH, 2)
                   |     <- (MALE, 1) 
                                      |     <- (MOLE, 2)
		                      |     <- (MILE, 2)  == We (minlevel = 2)  [PATH = {MILE, MALE, MATE}]
                   |     <- (LATE, 1)
		                      |     <- (LAKE, 2)

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
  DAG:   (HIT, 0)


Level: 1
   (Add all words that are one letter away from HIT)
   DAG:  (HIT, 0)
                   |     <- (HOT, 1)
   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
		   
Level: 2
   (Add all words that are one letter away from HOT)
   DAG:  (HIT, 0)
                   |     <- (HOT, 1)
                                     |     <- (DOT, 2)
                                     |     <- (LOT, 2)
   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
   [DOT]: (HOT, 2)
   [LOT]: (HOT, 2)

Level: 3
   (Add all words that are one letter away from DOT)
   DAG:  (HIT, 0)
                   |     <- (HOT, 1)
                                     |     <- (DOT, 2)
                                                       |     <- (DOG, 3)
                                     |     <- (LOT, 2)
   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
   [DOT]: (HOT, 2)
   [LOT]: (HOT, 2)
   [DOG]: (DOT, 3)

   (Add all words that are one letter away from LOT)
   DAG:  (HIT, 0)
                   |     <- (HOT, 1)
                                     |     <- (DOT, 2)
                                                       |     <- (DOG, 3)
                                     |     <- (LOT, 2)
                                                       |     <- (LOG, 3)
   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
   [DOT]: (HOT, 2)
   [LOT]: (HOT, 2)
   [DOG]: (DOT, 3)
   [LOG]: (LOT, 3)

Level: 4
   (Add all words that are one letter away from DOG)
   DAG:  (HIT, 0)
                   |     <- (HOT, 1)
                                     |     <- (DOT, 2)
                                                       |     <- (DOG, 3)
                                                                         |     <- (COG, 4)
                                     |     <- (LOT, 2)
                                                       |     <- (LOG, 3, LOT)
  
   AdjList:
   [HIT]:
   [HOT]: (HIT, 1)
   [DOT]: (HOT, 2)
   [LOT]: (HOT, 2)
   [DOG]: (DOT, 3)
   [LOG]: (LOT, 3)
   [COG]: (DOG, 4) == We (minlevel = 4)

 *Path: {COG, DOG, DOT, HOT, HIT}*

   (Add all words that are one letter away from DOG)
   DAG:  (HIT, 0)
                   |     <- (HOT, 1)
                                     |     <- (DOT, 2)
                                                       |     <- (DOG, 3)
                                                                         |     <- (COG, 4)
                                     |     <- (LOT, 2)
                                                       |     <- (LOG, 3, LOT)
                                                                         |     <- (COG, 4) 

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
   (Add all words that are one letter away from COG)
'''

'''
Lookup table to get one-letter transforms of any given word in list

words: MALE, MAKE, MIKE, PALE
u create lookup table initially like so
MALE:
  [*ALE]: {MALE}
  [M*LE]: {MALE}
  [MA*E]: {MALE}
  [MAL*]: {MALE}
MAKE:
  [*AKE]: {MAKE}
  [M*KE]: {MAKE}
  [MA*E]: {MALE, MAKE}
  [MAK*]: {MAKE}
MIKE:
  [*IKE]: {MIKE}
  [M*KE}: {MAKE, MIKE}
  [MI*E]: {MIKE}
  [MIK*]: {MIKE}
PALE:
  [*ALE]: {MALE, PALE}
  [P*LE]: {PALE}
  [PA*E]: {PALE}
  [PAL*]: {PALE}

Final lookup table:
  [M*LE]: {MALE}
  [MAL*]: {MALE}
  [*AKE]: {MAKE}
  [MA*E]: {MALE, MAKE}
  [MAK*]: {MAKE}
  [*IKE]: {MIKE}
  [M*KE}: {MAKE, MIKE}
  [MI*E]: {MIKE}
  [MIK*]: {MIKE}
  [*ALE]: {MALE, PALE}
  [P*LE]: {PALE}
  [PA*E]: {PALE}
  [PAL*]: {PALE}

To lookup for 'neighbors' of PALE
just search for *ALE, P*LE, PA*E and PAL*
'''

from Queue import Queue

class Solution(object):
	# Initialize a lookup-table to quickly get one-letter transforms of a given word
	def initialize_lookup_table(self, wordList):
		lookup_table = {}
		for word in wordList:
			for i in xrange(len(word)):
				# Create different placeholder keys for lookup
				key = word[:i] + '*' + word[i+1:]
				if lookup_table.get(key) is None:
					# Initialize an empty set incase the key doesnt already exist
					# Using a set so incase there are duplicates in wordlist
					# (problem statement says there arent)
					# OR
					# beginWord already exists in wordList
					# in which case, we dont want duplicate entries in the lookup table
					lookup_table[key] = set()
				lookup_table[key].add(word)

		return lookup_table


	# Find all words in the wordList that are one letter
	# away from 'word'
	def findOneLetterTranforms(self, word, lookup_table, visited):
		transforms = []
		for i in xrange(len(word)):
			key = word[:i] + '*' + word[i+1:]
			words = lookup_table[key]
			for w in words:
				if w != word and not visited.get(w):
					transforms.append(w)

		return transforms


	def ladderLength(self, beginWord, endWord, wordList):
		"""
		:type beginWord: str
		:type endWord: str
		:type wordList: List[str]
		:rtype: int
		"""

		# Add beginWord to the list, just in case
		# wordList doesn't have it already
		wordList.insert(0, beginWord)

		lookup_table = self.initialize_lookup_table(wordList)
		visited = {}
		bfs_q = Queue(len(wordList)+1)

		bfs_q.put_nowait((beginWord, 0)) # Each 'vertex' in the graph has (parent word index, Level in the graph)
		visited[beginWord] = True  # Mark startword as visited

		while not bfs_q.empty():
			(word, curr_level) = bfs_q.get_nowait()

			oneLetterTranforms = self.findOneLetterTranforms(word, lookup_table, visited)

			curr_level += 1

			for w in oneLetterTranforms:
				if w == endWord:
					return (curr_level + 1)
				visited[w] = True
				bfs_q.put_nowait((w, curr_level))

		# couldn't find endWord in list or
		# a path from beginWord to endWord
		return 0


if __name__ == '__main__':
	s = Solution()

	ltable = s.initialize_lookup_table(["MALE", "MAKE", "MIKE", "PALE"])

	table = {
	'*AKE': ['MAKE'],
	'*ALE': ['MALE', 'PALE'],
	'*IKE': ['MIKE'],
	'M*KE': ['MAKE', 'MIKE'],
	'M*LE': ['MALE'],
	'MA*E': ['MALE', 'MAKE'],
	'MAK*': ['MAKE'],
	'MAL*': ['MALE'],
	'MI*E': ['MIKE'],
	'MIK*': ['MIKE'],
	'P*LE': ['PALE'],
	'PA*E': ['PALE'],
	'PAL*':  ['PALE']
	}

	i = 0
	for k, v in sorted(ltable.items()):
		assert table.has_key(k)
		assert set(table[k]) == v

	ltable = s.initialize_lookup_table(["mate", "meat", "math", "late", "male", "mile"])

	assert sorted(s.findOneLetterTranforms("mate", ltable, {})) == sorted(["math", "late", "male"])
	assert sorted(s.findOneLetterTranforms("male", ltable, {})) == sorted(["mate", "mile"])

	assert s.ladderLength("hit", "cog", ["hot","dot","dog","lot","log"]) == 0
	assert s.ladderLength("hit", "cog", ["hot","dot","dog","lot","log","cog"]) == 5
	assert s.ladderLength("red", "tax", ["ted","tex","red","tax","tad","den","rex","pee"]) == 4
	assert s.ladderLength("mate", "mile", ["math","path","male","mole","mile","late", "lake", "like"]) == 3

	from test_in import we, ws, wl
	assert s.ladderLength(ws, we, wl) == 20

