#encoding: utf-8
'''
https://www.interviewbit.com/problems/sorted-insert-position/

Sorted Insert Position

Given a sorted array and a target value, return the index if the target is found. If not, return the index where it would be if it were inserted in order.

You may assume no duplicates in the array.

Here are few examples.

[1,3,5,6], 5 → 2
[1,3,5,6], 2 → 1
[1,3,5,6], 7 → 4
[1,3,5,6], 0 → 0
'''

'''
Solution Outline: O(log n)
	1. Use binary search to locate B in A
	2. Keep updating each position > B as a potential insert-index
	   Last-stored insert-index is the position to return
'''
class Solution:
	# @param A : list of integers
	# @param B : integer
	# @return an integer
	def searchInsert(self, A, B):
		if not A:
			return 0

		l, h = 0, len(A)-1
		pos = len(A)
		while l <= h:
			mid = (l+h)/2
			if A[mid] == B:
				return mid

			if A[mid] < B:
				# B might be in the right subarray
				l = mid+1
			else:
				# A[mid] > B
				# B might be in the left subarray
				# Store last position > B
				# The last number > B will the index
				# B will be inserted in
				pos = mid
				h = mid-1

		return pos


if __name__ == '__main__':
	s = Solution()
	assert s.searchInsert([1,3,5,6], 5) == 2 
	assert s.searchInsert([1,3,5,6], 2) == 1
	assert s.searchInsert([1,3,5,6], 7) == 4
	assert s.searchInsert([1,3,5,6], 0) == 0


