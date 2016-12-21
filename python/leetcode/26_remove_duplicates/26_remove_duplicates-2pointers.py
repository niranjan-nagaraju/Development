'''
https://leetcode.com/problems/remove-duplicates-from-sorted-array/

Given a sorted array, remove the duplicates in place such that each element
appear only once and return the new length.
i. It doesn't matter what you leave beyond the new length. 

[1,1,2] => 2
'''
class Solution(object):
    def removeDuplicates(self, nums):
		"""
		:type nums: List[int]
		:rtype: int
		"""

		if nums == []:
			return 0

		i = 0
		j = 1
		while True:
			try:
				if (nums [j] != nums[i]):
					i += 1
					nums[i] = nums[j]

				print nums
				j = j + 1

			except IndexError:
				break
	
		return i+1

obj = Solution()
l = [1,1,2,3]
print obj.removeDuplicates(l), l


''' 
Testcases:
TC: 1
l = [1,1,2,3]
[1, 1, 2, 3]
[1, 2, 2, 3]
[1, 2, 3, 3]
3 [1, 2, 3, 3]

TC: 2
l = [1,1,1,2,2,2,3,4,4,4]
[1, 1, 1, 2, 2, 2, 3, 4, 4, 4]
[1, 1, 1, 2, 2, 2, 3, 4, 4, 4]
[1, 2, 1, 2, 2, 2, 3, 4, 4, 4]
[1, 2, 1, 2, 2, 2, 3, 4, 4, 4]
[1, 2, 1, 2, 2, 2, 3, 4, 4, 4]
[1, 2, 3, 2, 2, 2, 3, 4, 4, 4]
[1, 2, 3, 4, 2, 2, 3, 4, 4, 4]
[1, 2, 3, 4, 2, 2, 3, 4, 4, 4]
[1, 2, 3, 4, 2, 2, 3, 4, 4, 4]
4 [1, 2, 3, 4, 2, 2, 3, 4, 4, 4]


TC: 3
l = [1,1,1,1,2,2,3,4,4,4]
[1, 1, 1, 1, 2, 2, 3, 4, 4, 4]
[1, 1, 1, 1, 2, 2, 3, 4, 4, 4]
[1, 1, 1, 1, 2, 2, 3, 4, 4, 4]
[1, 2, 1, 1, 2, 2, 3, 4, 4, 4]
[1, 2, 1, 1, 2, 2, 3, 4, 4, 4]
[1, 2, 3, 1, 2, 2, 3, 4, 4, 4]
[1, 2, 3, 4, 2, 2, 3, 4, 4, 4]
[1, 2, 3, 4, 2, 2, 3, 4, 4, 4]
[1, 2, 3, 4, 2, 2, 3, 4, 4, 4]
4 [1, 2, 3, 4, 2, 2, 3, 4, 4, 4]

'''
