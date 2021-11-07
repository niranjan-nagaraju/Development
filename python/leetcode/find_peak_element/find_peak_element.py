#encoding: utf-8
'''
https://leetcode.com/problems/find-peak-element/

162. Find Peak Element

A peak element is an element that is strictly greater than its neighbors.

Given an integer array nums, find a peak element, and return its index. If the array contains multiple peaks, return the index to any of the peaks.

You may imagine that nums[-1] = nums[n] = -∞.

You must write an algorithm that runs in O(log n) time.

 
Example 1:
 Input: nums = [1,2,3,1]
 Output: 2
 Explanation: 3 is a peak element and your function should return the index number 2.

Example 2:
 Input: nums = [1,2,1,3,5,6,4]
 Output: 5
 Explanation: Your function can return either index number 1 where the peak element is 2, or index number 5 where the peak element is 6.

Constraints:
  1 <= nums.length <= 1000
  -2^31 <= nums[i] <= 2^31 - 1
  nums[i] != nums[i + 1] for all valid i.
'''


'''
Solution Outline:
	0. mid = (l+h)/2
	1. if a[mid], a[mid+1] is increasing, then there is definitely a peak to the right.
		  a[mid+1..h]
	2. else, a[mid], a[mid+1] is a non-increasing sequence,
		there is definitely a peak to the left. a[l..mid]
'''
class Solution(object):
	def findPeakElement(self, lst):
		"""
		:type nums: List[int]
		:rtype: int
		"""
		l, h = 0, len(lst)-1
		while l < h:
			mid = l + (h-l)/2
			if lst[mid+1] > lst[mid]:
				l = mid+1
			else:
				h = mid
		return l 


if __name__ == '__main__':
	s = Solution()
	s.findPeakElement([1,2,3,1]) == 2
	s.findPeakElement([1,2,1,3,5,6,4]) in [1,5]

