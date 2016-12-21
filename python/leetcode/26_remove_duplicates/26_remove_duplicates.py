'''
https://leetcode.com/problems/remove-duplicates-from-sorted-array/

Given a sorted array, remove the duplicates in place such that each element
appear only once and return the new length.

[1,1,2] => 2
'''
class Solution(object):
    def removeDuplicates(self, nums):
		"""
		:type nums: List[int]
		:rtype: int
		"""
		count = 0
		i = 0
		while True:
			try:
				c = nums[i]
				count += 1
				i = i + 1

				while (nums[i] == c):
					del nums[i]
				
			except IndexError:
				break
	
		return count

obj = Solution()
l = [1,1,2,2,3]
print obj.removeDuplicates(l), l

        
