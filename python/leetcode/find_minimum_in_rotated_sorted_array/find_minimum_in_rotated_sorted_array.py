'''
https://leetcode.com/problems/find-minimum-in-rotated-sorted-array/

153. Find Minimum in Rotated Sorted Array

Suppose an array sorted in ascending order is rotated at some pivot unknown to you beforehand.

(i.e.,  [0,1,2,4,5,6,7] might become  [4,5,6,7,0,1,2]).

Find the minimum element.
You may assume no duplicate exists in the array.

Example 1:
Input: [3,4,5,1,2] 
Output: 1

Example 2:
Input: [4,5,6,7,0,1,2]
Output: 0
'''

# Given a rotated sorted array, return the index
# where the rotation pivot is (i.e. the smallest item in the array)
class Solution(object):
	def findMin(self, lst):
		"""
		:type lst: List[int]
		:rtype: int
		"""

		# Convenience lambda function to return index which contains the minimim of lst[i] vs lst[j]
		minIndex = lambda i, j: i if lst[i] < lst[j] else j

		l, h = 0, len(lst)-1
		pivot_candidate = l
		while l < h:
			if lst[l] < lst[h]:
				# Current window [l, h] is in sorted order
				# Check if the minimum element exists in lst[l]
				# If so, we are done here since we keep narrowing down the window
				# to contain pivot,
				# We either just moved past the pivot, or the pivot is in the current window
				#   If we had moved past, return last-saved pivot candidate
				#   If the current window has the pivot, it'd be lst[l] since lst[l..h] is in sorted order
				pivot_candidate = minIndex(l, pivot_candidate)
				break

			mid = (l+h)/2
			if lst[l] > lst[mid]:
				# pivot is in the left half
				pivot_candidate = mid
				h = mid-1
			elif lst[mid] > lst[h]:
				# pivot is in the right half
				pivot_candidate = h
				l = mid+1

		return lst[pivot_candidate]



if __name__ == '__main__':
	s = Solution()
	assert s.findMin([4,5,6,7,0,1,2]) == 0
	assert s.findMin([3,4,5,1,2]) == 1
	assert s.findMin([11,1,2,3,4,5,6,7,8,9,10]) == 1
	assert s.findMin([4,5,6,1,2,3]) == 1
	assert s.findMin([4,5,1,2,3]) == 1
	assert s.findMin([6,7,3,4,5]) == 3
	assert s.findMin([2,3,4,5,6]) == 2 # no rotation
	assert s.findMin([1,3,5]) == 1 # no rotation

