'''
https://leetcode.com/problems/search-in-rotated-sorted-array-ii/

81. Search in Rotated Sorted Array II

Suppose an array sorted in ascending order is rotated at some pivot unknown to you beforehand.

(i.e., [0,0,1,2,2,5,6] might become [2,5,6,0,0,1,2]).

You are given a target value to search. If found in the array return true, otherwise return false.

Example 1:
Input: nums = [2,5,6,0,0,1,2], target = 0
Output: true

Example 2:
Input: nums = [2,5,6,0,0,1,2], target = 3
Output: false

Follow up:
This is a follow up problem to Search in Rotated Sorted Array, where nums may contain duplicates.
Would this affect the run-time complexity? How and why?
'''


'''
Solution 1:
  Find pivot, and run a modified binary search shifting by 'pivot' units
'''
class Solution(object):
	def search(self, nums, target):
		"""
		:type nums: List[int]
		:type target: int
		:rtype: bool
		"""

		# Given a rotated sorted array, return the index
		# where the rotation pivot is (i.e. the smallest item in the array)
		def find_pivot(lst):
			# Convenience lambda function to return index which contains the minimim of lst[i] vs lst[j]
			minIndex = lambda i, j: i if lst[i] < lst[j] else j

			l, h = 0, len(lst)-1
			pivot_candidate = l
			while l <= h:
				# Down to last 2 elements
				# If they are both equql, return the first of the pair
				# as the pivot
				if l == h-1:
					if lst[l] == lst[h]:
						return l

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
						#h = h-1
					else:
						# lst[l] < lst[h]
						# Either l is the new minimum or we have already found the minimum
						#  In any case, return immediately
						break
				else: # Current window [l, h] is still rotated
					mid = (l+h)/2
					if lst[l] > lst[mid]:
						# pivot is in the left half
						pivot_candidate = mid
						h = mid-1
					elif lst[mid] > lst[h]:
						# pivot is in the right half
						pivot_candidate = h
						l = mid+1

			return pivot_candidate


		'''
		Solution #1:
			1. Find a pivot in O(logn) time
			2. Then do binary search that accounts for indices that wrap around in a circular manner.
		'''
		def binary_search_rotated(lst, key):
			pivot = find_pivot(lst)

			n = len(lst)

			l, h = 0, n-1
			while l <= h:
				mid_ = (l+h)/2

				# adjust mid to shift by 'pivot' units
				mid = (mid_ + pivot) % n

				if lst[mid] == key:
					return True
				elif key > lst[mid]:
					# key is in the right half
					l = (mid_ + 1)
				else: # key < lst[mid]
					# key is in the left half
					h = (mid_ - 1)

			# couldn't find key
			return False


		# Call helper function
		return binary_search_rotated(nums, target)



if __name__ == '__main__':
	s = Solution()
	assert s.search([4,5,6,7,0,1,2], 0) == True
	assert s.search([4,5,6,7,0,1,2], 3) == False

	assert s.search([4,5,6,7,1,2], 1) == True
	assert s.search([4,5,6,7,1,2], 2) == True
	assert s.search([4,5,6,7,1,2], 0) == False
	assert s.search([4,5,6,7,1,2], 4) == True
	assert s.search([4,5,6,7,1,2], 5) == True
	assert s.search([4,5,6,7,1,2], 6) == True
	assert s.search([4,5,6,7,1,2], 7) == True

	assert s.search([4,5,1,2,3], 4) == True
	assert s.search([4,5,1,2,3], 5) == True
	assert s.search([4,5,1,2,3], 1) == True
	assert s.search([4,5,1,2,3], 2) == True
	assert s.search([4,5,1,2,3], 3) == True
	assert s.search([4,5,1,2,3], 6) == False
	assert s.search([4,5,1,2,3], 0) == False

	assert s.search([3,1,3], 0) == False
	assert s.search([3,1,3], 3) == True
	assert s.search([3,1,3], 1) == True

	assert s.search([2,2,2,0,1], 2) == True
	assert s.search([2,2,2,0,1], 0) == True
	assert s.search([2,2,2,0,1], 1) == True
	assert s.search([2,2,2,0,1], -1) == False
	assert s.search([2,2,2,0,1], 3) == False

	# Traditional binary search on an unrotated sorted array
	# should work just as well
	assert s.search([1,2,3,4,5,6,7], 1) == True
	assert s.search([1,2,3,4,5,6,7], 2) == True
	assert s.search([1,2,3,4,5,6,7], 3) == True
	assert s.search([1,2,3,4,5,6,7], 4) == True
	assert s.search([1,2,3,4,5,6,7], 5) == True
	assert s.search([1,2,3,4,5,6,7], 6) == True
	assert s.search([1,2,3,4,5,6,7], 7) == True

	assert s.search([2,5,6,0,0,1,2], 0) == True
	assert s.search([2,5,6,0,0,1,2], 2) == True
	assert s.search([2,5,6,0,0,1,2], 5) == True
	assert s.search([2,5,6,0,0,1,2], 1) == True

