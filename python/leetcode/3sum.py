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

class Solution(object):
	def threeSum(self, nums):
		"""
		:type nums: List[int]
		:rtype: List[List[int]]
		"""
		return self.find_triplet(nums)

	# Find all pairs that adds in 'array' that adds upto a given 'target_sum'
	# O(n), single-pass
	@staticmethod
	def find_pairs(array, startidx, target_sum):
		# Create a reverse lookup map that returns index i, for array[i]
		lookup_map = {}
		pairs = []
		# Create reverse lookup as we match
		for i in range(startidx, len(array)):
			delta = target_sum - array[i]
			if lookup_map.has_key(delta):
				pairs.append((array[lookup_map[delta]], array[i]))
			else:
				# Add only the first item in the pair
				# without its matching delta added too
				# e.g. [1,4,1], sum = 5
				# add 1, when 4 matches sum (1+4), just add it to matching pairs,
				# without adding 4 to lookup map, so when at 1 again if we check
				# for sum, it should fail and result in no duplicates
				# so only (1,4) will be returned instead of (1,4), (4.1)
				lookup_map[array[i]] = i

		return pairs



	# Add a triplet (a,b,c) to the triplets list if it doesn't already exist
	# in the list (their relative order notwithstanding)
	# (1, 0, -1) == (1, -1, 0) == (-1, 0, 1)
	@staticmethod
	def add_unique_triplets(triplets, a, b, c):
		sorted_triplet = sorted([a,b,c])
		for t in triplets:
			# return without adding as soon as we find another triplet
			# with the same digits
			if sorted(t) == sorted_triplet:
				return

		# Entire list didn't have the same triplet (in whatever order)
		triplets.append([a,b,c])


	# find triplets (a,b,c) in the array that add upto to 0
	# a+b+c == 0, b+c == -a,
	# Use find_pair(array, -a) to find a pair that adds upto -(b+c)
	@staticmethod
	def find_triplet(array):
		triplets = []

		# keep a lookup table of all pairs for a given number 'n', results in sum '-n'
		lookup_table = {}
		for i in range(len(array)):
			# if we have already found all triplets involving array[i]
			# Don't look any further
			if not lookup_table.has_key(array[i]):
				pairs = Solution.find_pairs(array, i+1, -array[i])
				if pairs:
					for (a,b) in pairs:
						Solution.add_unique_triplets(triplets, array[i], a, b)
				lookup_table[array[i]] = True

		return triplets



if __name__ == '__main__':
	sol = Solution()
	assert sol.find_pairs([1,2,3,4,5,1], 0, 5) == [(2,3), (1,4)]
	assert sol.threeSum([-1, 0, 1, 2, -1, -4]) == [[-1, 0, 1], [-1, 2, -1]]
	assert sol.threeSum([1,2,3,-1,0]) == [[1, -1, 0]]

