'''
https://www.interviewbit.com/problems/rotated-sorted-array-search/

Rotated Sorted Array Search

Given an array of integers A of size N and an integer B.

array A is rotated at some pivot unknown to you beforehand.

(i.e., 0 1 2 4 5 6 7 might become 4 5 6 7 0 1 2 ).

You are given a target value B to search. If found in the array, return its index, otherwise return -1.

You may assume no duplicate exists in the array.

NOTE:- Array A was sorted in non-decreasing order before rotation.

NOTE : Think about the case when there are duplicates. Does your current solution work? How does the time complexity change?*

Input Format
The first argument given is the integer array A.
The second argument given is the integer B.

Output Format
Return index of B in array A, otherwise return -1

Constraints
1 <= N <= 1000000
1 <= A[i] <= 10^9
all elements in A are disitinct.

For Example
Input 1:
    A = [4, 5, 6, 7, 0, 1, 2, 3]
    B = 4
Output 1:
    0
Explanation 1:
 Target 4 is found at index 0 in A.


Input 2:
    A = [5, 17, 100, 3]
    B = 6
Output 2:
    -1
'''

'''
Solution Outline:
	1. Proceed as with a regular binary search, eliminating the half-subarray which might not contain the key
	2. If the subarray, A[l:h] is sorted, A[l] < A[h]
	    A[l:h] will not contain key if key is outside of A[l] and A[h]

	
Sample run:
	A: [4,5,6,7,0,1,2,3], key=2
        0 1 2 3 4 5 6 7
	
	l=0, h=7
	mid=3
	A[mid] == 7 != 2
	A[l:h] is rotated
	A[l] < A[mid]? yes
	  2 is not in A[l:mid]
	  l = mid+1 == 4

	l=4, h=7
	A[l:h] is sorted
	binsearch(A, 4, 7, 2) == 6
'''

class Solution:
	def rotated_binary_search(self, A, B):
		def binary_search(A, l, h, B):
			while l<=h:
				mid=(l+h)/2
				if A[mid] == B:
					return mid

				if A[mid] < B:
					l = mid+1
				else:
					# A[mid] > B
					h = mid-1

			return -1

		if not A:
			return -1

		l, h = 0, len(A)-1
		while l <= h:
			mid = (l+h)/2
			if A[mid] == B:
				return mid

			if A[l] < A[h]:
				return binary_search(A, l, h, B)

			if A[l] < A[mid]:
				# A[l:mid] is sorted

				# if B is outside of A[l:mid]
				# search in the other half
				if B > A[mid] or B < A[l]:
					l = mid+1
				else:
					# B is in the current subarray
					h = mid-1
			else: # A[mid] < A[h]
				# A[mid:h] is sorted

				# If B is outside of A[mid:h]
				# search for B in the other half
				if B < A[mid] or B > A[h]:
					h = mid-1
				else:
					# Look for B within the current subarray
					l = mid+1

		# Couldn't find key in a rotated sub-array
		return -1



if __name__ == '__main__':
	s = Solution()
	assert s.rotated_binary_search([4,5,6,7,0,1,2,3], 2) == 6
	assert s.rotated_binary_search([4,5,6,7,0,1,2,3], 4) == 0
	assert s.rotated_binary_search([4,5,6,7,0,1,2,3], 5) == 1
	assert s.rotated_binary_search([4,5,6,7,0,1,2,3], 8) == -1
	assert s.rotated_binary_search([4,5,6,7,1,2,3], 0) == -1
	assert s.rotated_binary_search([4,5,6,7,0,1,2], 3) == -1
	assert s.rotated_binary_search([4,5,0,1,2], 4) == 0

