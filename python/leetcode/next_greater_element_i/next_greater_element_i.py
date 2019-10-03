'''
https://leetcode.com/problems/next-greater-element-i/

496. Next Greater Element I

You are given two arrays (without duplicates) nums1 and nums2 where nums1's elements are subset of nums2.
Find all the next greater numbers for nums1's elements in the corresponding places of nums2.

The Next Greater Number of a number x in nums1 is the first greater number to its right in nums2. If it does not exist, output -1 for this number.

Example 1:
Input: nums1 = [4,1,2], nums2 = [1,3,4,2].
Output: [-1,3,-1]
Explanation:
    For number 4 in the first array, you cannot find the next greater number for it in the second array, so output -1.
    For number 1 in the first array, the next greater number for it in the second array is 3.
    For number 2 in the first array, there is no next greater number for it in the second array, so output -1.

Example 2:
Input: nums1 = [2,4], nums2 = [1,2,3,4].
Output: [3,-1]
Explanation:
    For number 2 in the first array, the next greater number for it in the second array is 3.
    For number 4 in the first array, there is no next greater number for it in the second array, so output -1.

Note:
All elements in nums1 and nums2 are unique.
The length of both nums1 and nums2 would not exceed 1000.
'''

from collections import defaultdict
class Solution(object):
	def nextGreaterElement(self, nums1, nums2):
		"""
		:type nums: List[int]
		:type nums2: List[int]
		:rtype: List[int]
		"""
		stack = []

		# All items have nge of -1 by default
		nge2 = defaultdict(lambda: -1)

		# First find NGE for all elements in nums2
		for i in xrange(len(nums2)):
			while stack and nums2[stack[0]] < nums2[i]:
				x = stack.pop(0)
				nge2[nums2[x]] = nums2[i]
			stack.insert(0, i)

		nge = [-1] * len(nums1)
		for i in xrange(len(nums1)):
			nge[i] = nge2[nums1[i]]

		return nge


if __name__ == '__main__':
	s = Solution()
	assert s.nextGreaterElement([4,1,2], [1,3,4,2]) == [-1,3,-1]
	assert s.nextGreaterElement([2,4], [1,2,3,4]) == [3,-1]

