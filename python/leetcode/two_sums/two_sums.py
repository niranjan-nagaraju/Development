#encoding: utf-8
'''
https://leetcode.com/problems/two-sum/
1. Two Sum
Given an array of integers, return indices of the two numbers such that they add up to a specific target.
You may assume that each input would have exactly one solution, and you may not use the same element twice.

Example:
	Given nums = [2, 7, 11, 15], target = 9,

	Because nums[0] + nums[1] = 2 + 7 = 9,
	return [0, 1].
'''

class Solution(object):
	"""
	:type nums: List[int]
	:type target: int
	:rtype: List[int]
	"""
	# Solution #1: O(n²)
	# Find the first i,j pair i>j s.t nums[i]+nums[j] == target
	def twoSum(self, nums, target):
		for i in xrange(len(nums)):
			for j in xrange(i+1, len(nums)):
				if target == nums[i]+nums[j]:
					return i,j

		return -1,-1 # Shouldn't happen as atleast one solution is guaranteed


	# Solution #2: Sort and find a,b s.t. nums[a]+nums[b] == target
	# O(nlogn)
	def twoSum_1(self, nums, target):
		# Find Indices i and j that contains key1 and key2 | i != j even if key1==key2
		def findIndices(nums, key1, key2):
			a,b=-1,-1
			for i in range(len(nums)):
				if (nums[i] == key1) and a == -1:
					a = i
				elif (nums[i] == key2):
					b = i

				i = i + 1

			return a,b


		nums_ = nums[:]
		nums_.sort()
		i = 0
		j = len(nums) - 1
		while ( i <= j ):
			c = nums_[i] + nums_[j]
			if target == c:
				return findIndices(nums, nums_[i], nums_[j])
			elif c < target:
				i += 1
			else:
				j -= 1

		return -1,-1



	# Solution #3: Create a reverse lookup table mapping nums[i] -> i
	# Then start matching b to (target-b) in the second pass
	# O(n), Two-pass
	def twoSum_2(self, nums, target):
		# Create a reverse lookup map that returns index i, for nums[i]
		lookup_map = {}
		for i in range(len(nums)):
			lookup_map[nums[i]] = i

		for i in range(len(nums)):
			c = target - nums[i]
			if lookup_map.has_key(c) and lookup_map[c] != i:
				return i, lookup_map[c]



	# Solution #4: Create a reverse lookup table mapping nums[i] -> i in-place
	# while matching b to (target-b) in a single pass
	# O(n), single-pass
	def twoSum_3(self, nums, target):
		# Create a reverse lookup map that returns index i, for nums[i]
		lookup_map = {}

		# Create lookup as we match
		# Trial run, {2, 7, 11, 15}, 18
		# 2 -> 16 does not exist, add 2 to map
		# 7 -> 11 does not exist(yet), add 7 to map
		# 11 -> 7 does exist, we have to return 7,11
		for i in range(len(nums)):
			c = target - nums[i]
			if lookup_map.has_key(c):
				# we return {map[c], i} instead of {i, map[c]} because
				# we might have missed returning {map[c], i} earlier as map[c] wasn't added yet
				return lookup_map[c], i 

			lookup_map[nums[i]] = i


	
	# Solution #5: Create a reverse lookup table mapping (target-nums[i]) -> i in-place
	# while matching target-b to b in a single pass
	# O(n), single-pass
	def twoSum_4(self, nums, target):
		# Create a reverse lookup map that returns index i, for nums[i]
		lookup_map = {}

		# Create lookup as we match
		# Trial run, {2, 7, 11, 15}, 18
		# 2 -> 16 does not exist, add 16 to map indicating we needed 16 to complete at index 0
		# 7 -> 11 does not exist(yet), add 11 to map at index 1
		# 11 -> 11 does exist, we have to return 7,11
		for i in range(len(nums)):
			c = target - nums[i]
			if lookup_map.has_key(nums[i]):
				# we return {map[c], i} instead of {i, map[c]} because
				# we might have missed returning {map[c], i} earlier as map[c] wasn't added yet
				return lookup_map[nums[i]], i 

			lookup_map[c] = i
			

if __name__ == '__main__':
	s = Solution()
	assert s.twoSum([1,2,3,4,5], 7) == (1,4)

	assert( s.twoSum([0,4,3,0], 0) ==
			s.twoSum_1([0,4,3,0], 0) ==
			s.twoSum_2([0,4,3,0], 0) ==
			s.twoSum_3([0,4,3,0], 0) ==
			s.twoSum_4([0,4,3,0], 0) ==
			(0,3))

	l = [2,7,11,15]
	assert( s.twoSum(l, 18) ==
			s.twoSum_1(l, 18) ==
			s.twoSum_2(l, 18) ==
			s.twoSum_3(l, 18) ==
			s.twoSum_4(l, 18) ==
			(1,2))
	assert( s.twoSum(l, 13) ==
			s.twoSum_1(l, 13) ==
			s.twoSum_2(l, 13) ==
			s.twoSum_3(l, 13) ==
			s.twoSum_4(l, 13) ==
			(0,2) )
	assert( s.twoSum(l, 9) ==
			s.twoSum_1(l, 9) ==
			s.twoSum_2(l, 9) ==
			s.twoSum_3(l, 9) ==
			s.twoSum_4(l, 9) ==
			(0,1) )

