#encoding: utf-8
'''
https://www.interviewbit.com/problems/search-for-a-range/

Search for a Range

Given a sorted array of integers A(0 based index) of size N, find the starting and ending position of a given integar B in array A.

Your algorithm’s runtime complexity must be in the order of O(log n).

Return an array of size 2, such that first element = starting position of B in A and second element = ending position of B in A, if B is not found in A return [-1, -1].


Input Format
The first argument given is the integer array A.
The second argument given is the integer B.

Output Format
 Return an array of size 2, such that first element = starting position of B in A and second element = ending position of B in A, if B is not found in A return [-1, -1].

Constraints
 1 <= N <= 10^6
 1 <= A[i], B <= 10^9

For Example
Input 1:
    A = [5, 7, 7, 8, 8, 10]
    B = 8
Output 1:
    [3, 4]
Explanation 1:
    First occurence of 8 in A is at index 3
    Second occurence of 8 in A is at index 4
    ans = [3, 4]

Input 2:
    A = [5, 17, 100, 111]
    B = 3
Output 2:
    [-1, -1]
'''


'''
Solution Outline: O(logn)
	1. Find thr first match, idx, for key in A
	2. Find leftmost occurence, l,  of key in A[0:idx]
	3. Find rightmost occurence, r,  of key in A[idx:n-1]
	4: Return [l, r]
'''
class Solution:
	# @param A : tuple of integers
	# @param B : integer
	# @return a list of integers
	def searchRange(self, A, B):
		# Search for B in A[l:h]
		# If direction is not specified, return index as
		# soon as a match is found
		# If direction is LEFTMOST, return the leftmost occurence of B in A
		# If direction is RIGHTMOST, return the rightmost occurence of B in A
		def binsearch(A, B, l, h, direction=None):
			idx = -1
			while l <= h:
				mid = (l+h)/2
				if A[mid]==B:
					idx = mid

					if direction is None:
						# No direction specified
						# return immediately
						return mid
					if direction == LEFTMOST:
						# Look for more occurences of B
						# to the left of idx
						h = mid-1
					else: # direction == RIGHTMOST
						# Look for more occurences of B
						# to the right of idx
						l = mid+1
				elif A[mid] > B:
					# Key is in the left half
					h = mid-1
				else:
					# A[mid] < B
					# Key is in the right half
					l = mid+1

			return idx

		if not A:
			return [-1. -1]

		LEFTMOST=1
		RIGHTMOST=2
		idx = binsearch(A, B, 0, len(A)-1)

		# B is not found in A
		if idx == -1:
			return [-1, -1]

		# Search for leftmost and rightmost occurences of B
		left = binsearch(A, B, 0, idx, LEFTMOST)
		right = binsearch(A, B, idx, len(A)-1, RIGHTMOST)
		return [left, right]

if __name__ == '__main__':
	s = Solution()
	assert s.searchRange([1,2,3,4,5,5,6], 5) == [4,5]
	assert s.searchRange([1,2,3,4,5,5,6], 6) == [6,6]
	assert s.searchRange([1,2,3,4,5,5,6], 3) == [2,2]
	assert s.searchRange([5, 7, 7, 8, 8, 10], 8) == [3,4]
	assert s.searchRange([5, 17, 100, 111], 3) == [-1,-1]

