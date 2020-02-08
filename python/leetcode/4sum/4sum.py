'''
https://leetcode.com/problems/4sum/

18. 4Sum

Given an array nums of n integers and an integer target, are there elements a, b, c, and d in nums such that a + b + c + d = target? Find all unique quadruplets in the array which gives the sum of target.

Note:

The solution set must not contain duplicate quadruplets.

Example:

Given array nums = [1, 0, -1, 0, -2, 2], and target = 0.

A solution set is:
[
  [-1,  0, 0, 1],
  [-2, -1, 1, 2],
  [-2,  0, 0, 2]
]
'''

'''
Solution Outline:
	1. Generate all sums of pairs (x,y) for nums
	   i, j | (nums[i] + nums[j])  (i < j)
	2. Identify (a,b) in  one of these pairs, and (c,d) in another
	   s.t a+b+c+d == target
	3. With these n*(n-1)/2 sum-pairs, Find two sums (s1, s2) s.t (s1+s2 == target)
	   let s1 have two candidates: (a,b), (c,d)
	   s2: (e,f), (g,h)
	   then (after clearing out duplicates),
	     quadruplets for target will be [(a,b,e,f), (a,b,g,h), (c,d,e,f), (c,d,g,h)]
	NOTE: (a,b) in s1 and (c,d) in s2 will not be an unique candidate if 
	       (a==c or d) or (b ==c or d)
'''

from collections import defaultdict
class Solution(object):
	def fourSum(self, nums, target):
		"""
		:type nums: List[int]
		:type target: int
		:rtype: List[List[int]]
		"""

		# Generate all pair-sums
		# and store the pairs that add upto the sums
		pairs_sums = defaultdict(lambda: [])
		for i in xrange(len(nums)):
			for j in xrange(i+1, len(nums)):
				pairs_sums[nums[i]+nums[j]].append((i,j))

		quadruplets = set()
		for i,v in pairs_sums.items():
			pair = pairs_sums.get(target-i)

			# s1: i, s2 s.t s1+s2 == target was not found
			if not pair:
				continue

			for a,b in pair:
				for x,y in v:
					# (a,b,x,y) is not unique
					# if a == x or a == y or b == x or b == y:
					if len(set([a,b,x,y])) != 4:
						continue

					(p1, p2, p3, p4) = sorted((nums[a], nums[b], nums[x], nums[y]))
					quadruplets.add((p1, p2, p3, p4))

		# Convert a set of tuples to list of lists
		result = map(list, quadruplets)
		return result




if __name__ == '__main__':
	s = Solution()
	assert s.fourSum([0,0,0,0], 0) == [[0,0,0,0]]
	assert s.fourSum([-1,3,2,0,1], 4) == [[-1,0,2,3]]
	assert s.fourSum([3,5,7,11,13,12,9,8,16,32], 26) == [[3,5,7,11]]
	assert s.fourSum([1, 0, -1, 0, -2, 2], 0) == [[-1, 0, 0, 1], [-2, -1, 1, 2], [-2, 0, 0, 2]]
	assert s.fourSum([10, 20, 30, 40, 1, 2], 91) == [[1, 20, 30, 40]]
	assert (sorted(s.fourSum(range(1,13), 18)) ==
		[[1, 2, 3, 12], [1, 2, 4, 11], [1, 2, 5, 10], [1, 2, 6, 9], [1, 2, 7, 8], [1, 3, 4, 10], [1, 3, 5, 9],
			[1, 3, 6, 8], [1, 4, 5, 8], [1, 4, 6, 7],
			[2, 3, 4, 9], [2, 3, 5, 8], [2, 3, 6, 7], [2, 4, 5, 7], [3, 4, 5, 6]])

