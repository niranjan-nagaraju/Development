'''
https://leetcode.com/problems/search-in-rotated-sorted-array/

33. Search in Rotated Sorted Array

Suppose an array sorted in ascending order is rotated at some pivot unknown to you beforehand.

(i.e., [0,1,2,4,5,6,7] might become [4,5,6,7,0,1,2]).

You are given a target value to search. If found in the array return its index, otherwise return -1.

You may assume no duplicate exists in the array.

Your algorithm's runtime complexity must be in the order of O(log n).

Example 1:
Input: nums = [4,5,6,7,0,1,2], target = 0
Output: 4

Example 2:
Input: nums = [4,5,6,7,0,1,2], target = 3
Output: -1
'''

'''
Solution #2: O(log n)
  Observations:
	1. A sub-array a[l..h] is rotated if a[l] > a[h]
	2. If a sub-array a[l..h] is rotated, then a[l..h] wont contain
	   any key, x, between a[h] .. a[l], i.e. a[h] < x < a[l]


	Proceed as with a regular binary search
	Consider an initial window [l,h] -> [0, n-1]

	Therefore,
	  check if a[l..h] is rotated, if not run classic binary search over [l..h]
	  If a[l..h] is infact rotated,
	    split in the middle, mid = (l+h)/2
		  if a[l] > a[mid], i.e. a[l..mid] is rotated
		     check if key is between a[mid] to a[l], if it is, skip this half.
			 if the key is outside of a[mid] .. a[l], then find key in this sub-array
		  otherwise, (a[mid..h] is rotated)
		     check if key is between a[h] to a[mid], if it is, skip this half.
			 if the key is outside of a[h] .. a[mid], then find key in this sub-array
'''
class Solution(object):
	def search(self, lst, key):
		"""
		:type lst: List[int]
		:type key: int
		:rtype: int
		"""

		# Traditional binary search
		def binary_search(lst, key, l, h):
			while l <= h:
				mid = (l+h)/2
				if lst[mid] == key:
					return mid
				elif key > lst[mid]:
					# search in second half of the sub-array
					l = mid+1
				else:
					# search in first half of the sub-array
					h = mid-1

			# Couldn't find 'key'
			return -1


		# Proceed as in traditional binary search
		l, h = 0, len(lst)-1

		while l <= h:
			if lst[l] < lst[h]:
				# run traditional binary search
				return binary_search(lst, key, l, h)

			# [l..h] is rotated
			mid = (l+h)/2
			if key == lst[mid]:
				return mid

			# Left half of the array is rotated
			if lst[l] > lst[mid]:
				# [l..mid] is rotated
				# => if key is between lst[mid] .. lst[l], lst[mid] < key < lst[l]
				# then it wont be found in [l..mid]
				# e.g., [5,6,1,2], key = 4
				# 2 < 4 < 5, therefore 4 wont be in [5,6,1,2]
				if lst[mid] < key < lst[l]:
					l = mid + 1
				else: # key is in current half
					h = mid - 1
			else: # lst[mid] >= lst[h] (or lst[l] == lst[mid])
				# [mid..h] is rotated
				# => if key is between lst[h] .. lst[mid], lst[h] < key < lst[mid]
				# then it wont be found in [mid..h]
				# e.g., [5,6,1,2], key = 4
				# 2 < 4 < 5, therefore 4 wont be in [5,6,1,2]
				if lst[h] < key < lst[mid]:
					h = mid - 1
				else: # key is in current half
					l = mid + 1

		return -1





if __name__ == '__main__':
	s = Solution()
	assert s.search([4,5,6,7,0,1,2], 0) == 4
	assert s.search([4,5,6,7,0,1,2], 3) == -1

	assert s.search([4,5,6,7,1,2], 1) == 4
	assert s.search([4,5,6,7,1,2], 2) == 5
	assert s.search([4,5,6,7,1,2], 0) == -1
	assert s.search([4,5,6,7,1,2], 4) == 0
	assert s.search([4,5,6,7,1,2], 5) == 1
	assert s.search([4,5,6,7,1,2], 6) == 2
	assert s.search([4,5,6,7,1,2], 7) == 3

	assert s.search([4,5,1,2,3], 4) == 0
	assert s.search([4,5,1,2,3], 5) == 1
	assert s.search([4,5,1,2,3], 1) == 2
	assert s.search([4,5,1,2,3], 2) == 3
	assert s.search([4,5,1,2,3], 3) == 4
	assert s.search([4,5,1,2,3], 6) == -1
	assert s.search([4,5,1,2,3], 0) == -1

	assert s.search([3,1,3], 0) == -1
	assert s.search([3,1,3], 3) in (0,2)
	assert s.search([3,1,3], 1) == 1

	assert s.search([2,2,2,0,1], 2) in (0,1,2) # either of 0,1,2 will do
	assert s.search([2,2,2,0,1], 0) == 3
	assert s.search([2,2,2,0,1], 1) == 4
	assert s.search([2,2,2,0,1], -1) == -1
	assert s.search([2,2,2,0,1], 3) == -1

	# Traditional binary search on an unrotated sorted array
	# should work just as well
	assert s.search([1,2,3,4,5,6,7], 1) == 0
	assert s.search([1,2,3,4,5,6,7], 2) == 1
	assert s.search([1,2,3,4,5,6,7], 3) == 2
	assert s.search([1,2,3,4,5,6,7], 4) == 3
	assert s.search([1,2,3,4,5,6,7], 5) == 4
	assert s.search([1,2,3,4,5,6,7], 6) == 5
	assert s.search([1,2,3,4,5,6,7], 7) == 6


