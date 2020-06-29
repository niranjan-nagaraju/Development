'''
https://www.interviewbit.com/problems/subset/

Subset

Given a set of distinct integers, S, return all possible subsets.

Note:
	Elements in a subset must be in non-descending order.
	The solution set must not contain duplicate subsets.
	Also, the subsets should be sorted in ascending ( lexicographic ) order.
	The list is not necessarily sorted.

Example :
If S = [1,2,3], a solution is:

[
  [],
  [1],
  [1, 2],
  [1, 2, 3],
  [1, 3],
  [2],
  [2, 3],
  [3],
]
'''


'''
Solution Outline:
	1. Sort the input, S,  to get a lexicographically sorted list
	2. Add [] empty subset to the powerset
	3. Start a DFS traversal beginning at level=0, with neighbors of level, i, as (i+1 .. n-1) {n: len(S)}
	   Each DFS call is sent a prefix of all the elements in this current path.
	   Backtrack by unwinding elements at each level after DFS for that level returns
	   At the end of the DFS traversal, all subsets beginning with S[0] will be generated.
	4. Repeat {step 2}. with level = 1, 2, 3, .. n-1 to get all the subsets (in lexicographically sorted order)

Sample run:
	S: [2,1,3]
	
	sort: S: [1,2,3]
	subsets = []

	DFS traversal(level=0)
	  prefix: [1], Add to subsets
	   subsets: []
				[1]
		DFS traversal(level=1)
		prefix: [1,2], Add to subsets
		 subsets: []
				  [1], [1,2]
			DFS traversal(level=2)
			prefix: [1,2,3], Add to subsets
			 subsets: []
					  [1], [1,2], [1,2,3]
		DFS traversal(level=1)
		prefix: [1,3], Add to subsets
		 subsets: []
				  [1], [1,2], [1,2,3], [1,3]

	DFS traversal(level=1)
	  prefix: [2], Add to subsets
	   subsets: []
				[1], [1,2], [1,2,3], [1,3]
				[2]
		DFS traversal(level=2)
		prefix: [2,3], Add to subsets
		 subsets: []
				  [1], [1,2], [1,2,3], [1,3]
				  [2], [2,3]

	DFS traversal(level=2)
	  prefix: [3], Add to subsets
	   subsets: []
				[1], [1,2], [1,2,3], [1,3]
				[2], [2,3]
				[3]
'''

class Solution:
	def enumerate_all_subsets(self, S):
		def dfs_subsets(level, prefix=[]):
			if level == len(S):
				return

			lexicographic_subsets.append(prefix+[S[level]])
			dfs_subsets(level+1, prefix+[S[level]])
			# backtrack, prefix is unchanged here

			dfs_subsets(level+1, prefix)

		S.sort()
		lexicographic_subsets = [[]]
		dfs_subsets(0)
		return lexicographic_subsets



if __name__ == '__main__':
	s = Solution()
	assert s.enumerate_all_subsets([]) == [[]]
	assert s.enumerate_all_subsets([1]) == [[], [1]]
	assert s.enumerate_all_subsets([1,2]) == [
												[],
												[1], [1,2],
												[2]
												]
	assert s.enumerate_all_subsets([1,2,3]) == [
												[],
												[1], [1,2], [1,2,3], [1,3],
												[2], [2,3],
												[3]
												]
	assert s.enumerate_all_subsets([1,2,3,4]) == [
													[],
													[1], [1,2], [1,2,3], [1,2,3,4], [1,2,4], [1,3], [1,3,4], [1,4],
													[2], [2,3], [2,3,4], [2,4],
													[3], [3,4],
													[4]
												  ]

