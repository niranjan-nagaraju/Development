'''
https://leetcode.com/problems/find-minimum-in-rotated-sorted-array-ii/submissions/

154. Find Minimum in Rotated Sorted Array II

Suppose an array sorted in ascending order is rotated at some pivot unknown to you beforehand.

(i.e.,  [0,1,2,4,5,6,7] might become  [4,5,6,7,0,1,2]).

Find the minimum element.

*The array may contain duplicates.*

Example 1:
Input: [1,3,5]
Output: 1

Example 2:
Input: [2,2,2,0,1]
Output: 0

Note:
This is a follow up problem to Find Minimum in Rotated Sorted Array.
Would allow duplicates affect the run-time complexity? How and why?
'''

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
		while l <= h:
			# Current window [l, h] is in sorted order
			# Check if the minimum element exists in lst[l]
			# If so, we are done here since we keep narrowing down the window
			# to contain pivot,
			# We either just moved past the pivot, or the pivot is in the current window
			#   If we had moved past, return last-saved pivot candidate
			#   If the current window has the pivot, it'd be lst[l] since lst[l..h] is in sorted order 
			if lst[l] <= lst[h]:
				pivot_candidate = minIndex(l, pivot_candidate)

				# If both ends of the window are equal, the pivot might still exist inside it
				# Check if either l,h can be a pivot candidate
				# and narrow the window down to exclude either ends.
				if lst[l] == lst[h]:
					l = l+1
					h = h-1
				else:
					# lst[l] < lst[h]
					# Either l is the new minimum or we have already found the minimum
					#  In any case, return immediately
					break

			else:  # Current window [l, h] is still rotated
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
	assert s.findMin([1]) == 1
	assert s.findMin([1,1]) == 1
	assert s.findMin([3,1,3]) == 1
	assert s.findMin([2,2,2,0,1]) == 0
	assert s.findMin([2,2,2,1,1]) == 1

	# Solution should work even if there aren't any duplicates
	# or if the sorted array wasn't rotated in the first place
	assert s.findMin([4,5,6,7,1,2]) == 1
	assert s.findMin([4,5,6,7,0,1,2]) == 0
	assert s.findMin([3,4,5,1,2]) == 1
	assert s.findMin([11,1,2,3,4,5,6,7,8,9,10]) == 1
	assert s.findMin([4,5,6,1,2,3]) == 1
	assert s.findMin([4,5,1,2,3]) == 1
	assert s.findMin([6,7,3,4,5]) == 3
	assert s.findMin([2,3,4,5,6]) == 2 # no rotation

