#encoding: utf-8
'''
https://www.interviewbit.com/problems/combination-sum-ii/

Combination Sum II

Given a collection of candidate numbers (C) and a target number (T), find all unique combinations in C where the candidate numbers sums to T.

Each number in C may only be used once in the combination.

Note:
 All numbers (including target) will be positive integers.
 Elements in a combination (a1, a2, … , ak) must be in non-descending order. (ie, a1 ≤ a2 ≤ … ≤ ak).
 The solution set must not contain duplicate combinations.
 
Example :
 Given candidate set 10,1,2,7,6,1,5 and target 8,

A solution set is:
	 [1, 7]
	 [1, 2, 5]
	 [2, 6]
	 [1, 1, 6]
'''


'''
Solution Outline:
	1. For each x in C, x is either part of the solution set, or it isn't.
	2. Let f(c, t) be a function that returns a solution for target sum, t, using c as the candidate set.
		Recursively solve f(c, t) as
			f(c-x, t-x) + f(c-x, t), for each x in C
		=> f(c-x, t-x): Solution set that includes x, 
		=> f(c-x, t): Solution set that excludes x.


Sample run:
	C: [1,2,5,7], target: 8

	L0: f([1,2,5,7], 8, []):
		L1: f([2,5,7], 7, [1]) + f([2,5,7], 8, [])
		L1: f([2,5,7, 7, [1])
			L2: f([5,7], 5, [1,2]) + f([5,7], 7, [1])
			L2: f([5,7], 5, [1,2])
				L3: f([7], 0, [1,2,5]) + f([7], 5, [1,2])
				L3: f([7], 0, [1,2,5]) <-- [1,2,5] is a candidate
				L3: f([7], 5, [1,2]) = f([], -2, [1,2,7]) + f([], 5, [1,2]) == []
			L2: f([5,7], 7, [1])
				L3: f([7], 2, [1,5]) + f([7], 7, [1])
				L3: f([7], 2, [1,5]) == []
				L3: f([7], 7, [1]) = f([], 0, [1,7]) <--- [1,7] is a candidate
		L1: f([2,5,7, 8, []) == []

	Solution: [[1,2,5], [1,7]]
'''
class Solution:
	def combinations_sum(self, C, target):
		def combinations_sum_(c, t, prefix=[]):
			if t == 0:
				# target-sum can achieved with prefix+C[0]
				# add to results, and backtrack to previous levels
				# backtrack to previous levels
				# because current level can only
				# have 1 candidate to match the target sum
				# NOTE: c can be [] at this point
				# so check if target sum was achieved first
				results.add(tuple(prefix))
				return

			if not c or t < 0:
				return
				# backtrack to previous levels
				# c is [] or
				# the lowest number in C > required target sum
				return

			combinations_sum_(c[1:], t-c[0], prefix+[c[0]]) # Include C[0]
			combinations_sum_(c[1:], t, prefix) # Exclude C[0]

		C.sort()
		# A set is used to de-dup entries incase C itself has duplicates
		# for e.g., if x == x',
		# in which case, [x, x', y] and [x', x, y] will be duplicate
		# entries in the results
		results = set()
		combinations_sum_(C, target)
		return sorted(map(list, results))


if __name__ == '__main__':
	s = Solution()
	assert s.combinations_sum([4,3,2,7], 7) == [ [3,4], [7] ]
	assert s.combinations_sum([2,3,6,7], 7) == [ [7] ]
	assert s.combinations_sum([10,1,2,7,6,1,5], 8) == [[1, 1, 6], [1, 2, 5], [1, 7], [2, 6]]
	assert s.combinations_sum([1,2,5,6,7,8], 8) == [[1, 2, 5], [1, 7], [2, 6], [8]]

