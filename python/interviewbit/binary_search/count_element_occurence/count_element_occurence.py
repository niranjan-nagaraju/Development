'''
https://www.interviewbit.com/problems/count-element-occurence/

Given a sorted array of integers, find the number of occurrences of a given target value.
Your algorithm's runtime complexity must be in the order of O(log n).
If the target is not found in the array, return 0

**Example : **
Given [5, 7, 7, 8, 8, 10] and target value 8,
return 2.
'''

'''
Solution Outline:
	1. Find the first index matching B in A
	2. Then look to left and right of this index for the leftmost and rightmost occurences
	3. Return the size of the window as the frequency of B in A
'''
class Solution:
	# @param A : tuple of integers
	# @param B : integer
	# @return an integer
	def findCount(self, A, B):
		LEFTMOST=1
		RIGHTMOST=2

		def binary_search(A, l, r, B, search_occurence=None):
			matchidx = None
			while l <= r:
				mid = (l+r)/2
				if A[mid] == B:
					matchidx = mid
					if search_occurence == None:
						# No search direction specified, return as soon as a match is found
						return matchidx
					elif search_occurence == LEFTMOST:
						# restrict new search window to left of current match
						r = mid-1
					else: # search_occurence == RIGHTMOST:
						# restrict new search window to right of current match
						l = mid + 1
				elif A[mid] < B:
					l = mid+1
				else: # A[mid] > B
					r = mid-1

			return matchidx


		# find the first index matching B in A
		# then look to left and right of this index for the leftmost and rightmost occurences
		# return the size of the window as the frequency of B in A
		first_match = binary_search(A, 0, len(A)-1, B)
		if first_match == None:
			return 0

		# search for leftmost occurence of element in A[0:first_match]  # first_match included
		lidx = binary_search(A, 0, first_match, B, LEFTMOST)
		# search for rightmost occurence of element in A[first_match:n-1]  # first_match included
		ridx = binary_search(A, first_match, len(A)-1, B, RIGHTMOST)

		return ridx-lidx+1


	
if __name__ == '__main__':
	s = Solution()
	assert s.findCount([1,1,3,3,5,5,5,5,5,9,9,11], 11) == 1
	assert s.findCount([1,1,3,3,5,5,5,5,5,9,9,11], 1) == 2
	assert s.findCount([1,1,3,3,5,5,5,5,5,9,9,11], 3) == 2
	assert s.findCount([1,1,3,3,5,5,5,5,5,9,9,11], 5) == 5
	assert s.findCount([1,1,3,3,5,5,5,5,5,9,9,11], 7) == 0
	assert s.findCount([1,1,3,3,5,5,5,5,5,9,9,11], 12) == 0
	assert s.findCount([1,1,3,3,5,5,5,5,5,9,9,11], 0) == 0
	assert s.findCount([5,5,5,5,5,5], 5) == 6

