'''
https://leetcode.com/problems/3sum/

Return all unique triplets that add upto 0
a + b + c = 0

Given array nums = [-1, 0, 1, 2, -1, -4],

A solution set is:
[
  [-1, 0, 1],
  [-1, -1, 2]
]
'''

'''
Solution Outline:
	1. Generate all pairs (a,b) and their sums, and store their indices in a lookup table
	2. For each x in nums, find if a pair with (target-x) exists with all (a,b,x) different
'''

from collections import defaultdict
class Solution(object):
	def threeSum(self, nums):
		"""
		:type nums: List[int]
		:rtype: List[List[int]]
		"""
		pairs_sums = defaultdict(lambda: [])
		for i in xrange(len(nums)):
			for j in xrange(i+1, len(nums)):
				pairs_sums[nums[i]+nums[j]].append((i,j))

		triplets = {}
		found = defaultdict(lambda: False)
		for i in xrange(len(nums)):
			x = nums[i]

			# we have already found a triplet involving [x]
			if found[x]:
				continue

			found[x] = True
			pairs = pairs_sums[-x]
			for a,b in pairs:
				# we know a<b
				# just check if i is not either of a or b so all triplets are unique
				# we need a triplet i < a < b
				if a <= i:
					continue

				a,b,c = sorted([nums[a],nums[b],x])
				triplets[(a,b,c)] = [a,b,c]

		return triplets.values()


if __name__ == '__main__':
	sol = Solution()
	assert sol.threeSum([0,0,0]) == [[0,0,0]]
	assert sol.threeSum([-1, 0, 1, 2, -1, -4]) == [[-1, -1, 2], [-1, 0, 1]]
	assert sol.threeSum([1,2,3,-1,0]) == [[-1, 0, 1]]

