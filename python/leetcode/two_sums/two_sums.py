'''
	https://leetcode.com/problems/two-sum/
	Example:
	Given nums = [2, 7, 11, 15], target = 9,

	Because nums[0] + nums[1] = 2 + 7 = 9,
	return [0, 1].
'''

class Solution(object):
	# Find Indices i and j that contains key1 and key2 | i != j even if key1==key2
	def findIndices(self, nums, key1, key2):
		a,b=-1,-1
		for i in range(len(nums)):
			if (nums[i] == key1) and a == -1:
				a = i
			elif (nums[i] == key2):
				b = i

			i = i + 1

		return a,b


	# O(nlogn)
	def twoSum_1(self, nums, target):
		"""
		:type nums: List[int]
		:type target: int
		:rtype: List[int]
		"""
		nums_ = nums[:]
		nums_.sort()
		i = 0
		j = len(nums) - 1
		while ( i <= j ):
			c = nums_[i] + nums_[j]
			if target == c:
				return self.findIndices(nums, nums_[i], nums_[j])
			elif c < target:
				i += 1
			else:
				j -= 1

		return 0,0

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
			

s = Solution()
assert( s.twoSum_1([0,4,3,0], 0) == s.twoSum_2([0,4,3,0], 0) == s.twoSum_3([0,4,3,0], 0) == (0,3) )

l = [2,7,11,15]
assert( s.twoSum_1(l, 18) == s.twoSum_2(l, 18) == s.twoSum_3(l, 18) == (1,2) )
assert( s.twoSum_1(l, 13) == s.twoSum_2(l, 13) == s.twoSum_3(l, 13) == (0,2) )
assert( s.twoSum_1(l, 9) == s.twoSum_2(l, 9) == s.twoSum_3(l, 9) == (0,1) )
