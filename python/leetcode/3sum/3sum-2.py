#encoding: utf-8

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
	Worst case performance would be O(n³)
	for i in 0 to n:
	  for j in i+1 to n:
	    for k in j+1 to n:
		   if i+j+k == 0:
		      solution += [i,j,k]

	
	Improve upon it by making it quadratic.
	1. Record all possible pairs of sums, (a+b), into a table
	2. For each c in array, find if -c exists in the table
'''

from collections import defaultdict
class Solution(object):
	def threeSum(self, nums):
		"""
		:type nums: List[int]
		:rtype: List[List[int]]
		"""
		return self.find_triplets(nums)

	
	# Add all pairs in the array, and record them in a lookup table
	@staticmethod
	def all_pairs(array):
		pairs = defaultdict(list)
		n = len(array)
		for i in xrange(n):
			for j in xrange(i+1, n):
				pairs[array[i]+array[j]].append((i, j))

		return pairs


	# Add a triplet (a,b,c) to the triplets list if it doesn't already exist
	# in the list (their relative order notwithstanding)
	# (1, 0, -1) == (1, -1, 0) == (-1, 0, 1)
	@staticmethod
	def add_unique_triplets(triplets, a, b, c):
		[a,b,c] = sorted([a,b,c])
		triplets[(a,b,c)] = [a,b,c]


	# find triplets (a,b,c) in the array that add upto to 0
	# a+b+c == 0, a+b == -c
	# Use table from all_pairs() to find a pair that adds upto -c for each c
	@staticmethod
	def find_triplets(array):
		pairs = Solution.all_pairs(array)
		triplets = {}
		lookup_table = defaultdict(bool)
		for k in xrange(len(array)):
			c = array[k]
			# if we have already found all triplets involving array[k]
			# Don't look any further
			if not lookup_table[c]:
				for pair in pairs[-c]:
					if k not in pair:
						a,b = array[pair[0]], array[pair[1]]
						Solution.add_unique_triplets(triplets, a, b, c)
				lookup_table[c] = True

		return triplets.values()



if __name__ == '__main__':
	sol = Solution()
	assert sol.threeSum([-1, 0, 1, 2, -1, -4]) == [[-1, -1, 2], [-1, 0, 1]]
	assert sol.threeSum([1,2,3,-1,0]) == [[-1, 0, 1]]
	assert sol.threeSum([0,0,0]) == [[0,0,0]]

