'''
https://www.interviewbit.com/problems/subsets-ii/

Subsets II

Given a collection of integers that might contain duplicates, S, return all possible subsets.

Note:
	Elements in a subset must be in non-descending order.
	The solution set must not contain duplicate subsets.
	The subsets must be sorted lexicographically.


Example :
If S = [1,2,2], the solution is:
	[
		[],
		[1],
		[1,2],
		[1,2,2],
		[2],
		[2, 2]
	]
'''

'''
Solution Outline:
	1. Sort the input, S,  to get a lexicographically sorted list
	2. Use a set{} to store unique subsets.
	   2.1 Add [] empty subset to the powerset
	3. Start a DFS traversal beginning at level=0, with neighbors of level, i, as (i+1 .. n-1) {n: len(S)}
	   Each DFS call is sent a prefix of all the elements in this current path.
	   Backtrack by unwinding elements at each level after DFS for that level returns
	   At the end of the DFS traversal, all subsets beginning with S[0] will be generated.
	4. Repeat {step 2}. with level = 1, 2, 3, .. n-1 to get all the subsets (in lexicographically sorted order)

Sample run:
	S: [2,1,2]
	
	sort: S: [1,2,2]
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
			prefix: [1,2,2], Add to subsets
			 subsets: []
					  [1], [1,2], [1,2,2]
		DFS traversal(level=1)
		prefix: [1,2], Add to subsets
		 subsets: []
				  [1], [1,2], [1,2,2]

	DFS traversal(level=1)
	  prefix: [2], Add to subsets
	   subsets: []
				[1], [1,2], [1,2,2]
				[2]
		DFS traversal(level=2)
		prefix: [2,2], Add to subsets
		 subsets: []
				  [1], [1,2], [1,2,2]
				  [2], [2,2]

	DFS traversal(level=2)
	  prefix: [2], Add to subsets
	   subsets: []
				[1], [1,2], [1,2,2]
				[2], [2,2]
'''
class Solution:
	def enumerate_distinct_subsets(self, S):
		def dfs_subsets(level):
			prefix.append(S[level]) # Add item at index 'level' to prefix
			lexicographic_subsets.add(tuple(prefix))
			for i in xrange(level+1, len(S)):
				dfs_subsets(i)
			prefix.pop() # backtrack

		S.sort()
		prefix = []
		lexicographic_subsets = set([()])
		for i in xrange(len(S)):
			# enumerate all distinct subsets starting from S[i]
			dfs_subsets(i)

		return sorted(map(lambda x: list(x), lexicographic_subsets))



if __name__ == '__main__':
	s = Solution()
	assert s.enumerate_distinct_subsets([1,2,2]) == [
												[],
												[1], [1,2], [1,2,2],
												[2], [2,2]
												]

	assert s.enumerate_distinct_subsets([]) == [[]]
	assert s.enumerate_distinct_subsets([1]) == [[], [1]]
	assert s.enumerate_distinct_subsets([1,2]) == [
												[],
												[1], [1,2],
												[2]
												]
	assert s.enumerate_distinct_subsets([1,2,3]) == [
												[],
												[1], [1,2], [1,2,3], [1,3],
												[2], [2,3],
												[3]
												]
	assert s.enumerate_distinct_subsets([1,2,3,4]) == [
													[],
													[1], [1,2], [1,2,3], [1,2,3,4], [1,2,4], [1,3], [1,3,4], [1,4],
													[2], [2,3], [2,3,4], [2,4],
													[3], [3,4],
													[4]
												  ]

