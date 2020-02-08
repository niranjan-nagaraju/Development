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
	3. Create a sorted list of these n*(n-1)/2 sum-pairs, Find two sums (s1, s2) s.t (s1+s2 == target)
	   using two pointers, one from the left and another from the right
	    (the same as finding all pairs that add upto sum in a sorted array)
	4. let s1 have two candidates: (a,b), (c,d)
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

		# Get a sorted list of all sums from every pair
		sums = sorted(pairs_sums.keys())

		# Now that the sums are sorted
		# Use two-pointers at either ends to narrow down pairs adding upto sum in a
		# sorted array
		i, j = 0, len(sums)-1
		while i <= j:
			# NOTE: i<=j because {[0,0,0,0],0} has only one sum
			# and will have multiple pairs adding upto 0 in the same pairs_sums
			# Also [2]: (2,0), (-1, 3)
			# The two pairs in [2] can be combined to make a quadruplet that adds upto 4
			# (2,0,-1,3)
			curr_sum = sums[i] + sums[j]
			if curr_sum < target:
				# Advance left window so current sum increases
				i += 1
			elif curr_sum > target:
				# Advance right window so current sum decreases
				j -= 1
			else: # curr_sum == target:
				pairs_i = pairs_sums[sums[i]]
				pairs_j = pairs_sums[sums[j]]

				for a,b in pairs_i:
					for x,y in pairs_j:
						# (a,b,x,y) is not unique
						# if a == x or a == y or b == x or b == y:
						if len(set([a,b,x,y])) != 4:
							continue

						(p1, p2, p3, p4) = sorted((nums[a], nums[b], nums[x], nums[y]))
						quadruplets.add((p1, p2, p3, p4))

				# Shrink current window to exclude both sums[i], sums[j] so other pairs can be found
				i += 1
				j -= 1

		# Convert a set of tuples to list of lists
		result = map(list, quadruplets)
		return result




if __name__ == '__main__':
	s = Solution()
	assert s.fourSum([0,0,0,0], 0) == [[0,0,0,0]]
	assert s.fourSum([3,5,7,11,13,12,9,8,16,32], 26) == [[3,5,7,11]]
	assert sorted(s.fourSum([1, 0, -1, 0, -2, 2], 0)) == sorted([[-1, 0, 0, 1], [-2, -1, 1, 2], [-2, 0, 0, 2]])
	assert s.fourSum([10, 20, 30, 40, 1, 2], 91) == [[1, 20, 30, 40]]
	assert (sorted(s.fourSum(range(1,13), 18)) ==
		[[1, 2, 3, 12], [1, 2, 4, 11], [1, 2, 5, 10], [1, 2, 6, 9], [1, 2, 7, 8], [1, 3, 4, 10], [1, 3, 5, 9],
			[1, 3, 6, 8], [1, 4, 5, 8], [1, 4, 6, 7],
			[2, 3, 4, 9], [2, 3, 5, 8], [2, 3, 6, 7], [2, 4, 5, 7], [3, 4, 5, 6]])

