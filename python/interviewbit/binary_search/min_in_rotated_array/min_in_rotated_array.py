'''
https://www.interviewbit.com/problems/rotated-array/

Rotated Array

Suppose a sorted array A is rotated at some pivot unknown to you beforehand.

(i.e., 0 1 2 4 5 6 7 might become 4 5 6 7 0 1 2).

Find the minimum element.

The array will not contain duplicates.
'''


'''
Solution Outline:
	The min element will also be the pivot around which the sorted array is rotated.
	If the pivot element is at index i, then A[i] < A[i-1]
	To find the pivot element,
	  Split the array into two,
	    mid = (l+r)/2, candidate = A[l]
		if A[l] < A[r]:
		   A[l:r] is sorted, return min(A[l], candidate found so far)

		Otherwise, A[l:r] is rotated	
			if A[l] > A[mid]:
				pivot is in the left half, Narrow search to left half
				candidate = A[mid]
				r = mid-1
			if A[mid] > A[r]:
				pivot is in the right half, Narrow search to right half
				candidate = A[r]
				l = mid+1


Sample run:
	A: 4 5 6 7 0 1 2
	   0 1 2 3 4 5 6

	l = 0, r = 6
	A[l] > A[r] => rotated
	mid = 3
	A[mid] = 7
	A[mid] > A[r] => pivot is in the right half
	candidate = A[r] == 2
	l = mid+1 == 4

	l = 4, r = 6
	A[l:r] is sorted, return min(A[l],candidate) = min(A[4],2) == min(0,2) == 0 as the minimum


Sample run 2:
	A = [4,5,6,7,1,2]
	     0 1 2 3 4 5

	l = 0, r = 5
	mid = 2
	A[l] > A[r] => rotated
	A[mid] > A[r] => pivot is in the right half
	candidate = A[r] = 2
	l = mid+1 = 3

	l = 3, r = 5
	mid = 4
	A[l] > A[r] => rotated
	A[l] > A[mid] => pivot is in the left half
	candidate = A[mid] = 1
	r = mid-1 == 4

	l = 3, r = 4
	mid = 3
	A[l] > A[r] => rotated
	A[mid] > A[r] => pivot is in the right half
	candidate = A[r] = 1
	l = mid+1 == 4

	l == r
	=> return candidate
'''

class Solution:
	def min_rotated_array(self, A):
		candidate = A[0]
		l, r = 0, len(A)-1
		while l < r:
			if A[l] < A[r]:
				# sub-array A[l:h] is sorted
				# return minimum(candidate found so far, A[l])
				return min(candidate, A[l])

			# A[l:r] is rotated
			mid = (l+r)/2
			if A[l] > A[mid]:
				# pivot is in the left half, Mark A[mid] as a candidate
				# and look at A[l:mid-1]
				candidate = A[mid]
				r = mid-1
			else: # A[mid] > A[r]
				# pivot is in the right half, Mark A[r] as a candidate
				# and look at A[mid+1, r]
				candidate = A[r]
				l = mid+1

		return candidate


if __name__ == '__main__':
	s = Solution()
	assert s.min_rotated_array([4,5,6,7,0,1,2]) == 0
	assert s.min_rotated_array([4,5,6,7,1,2]) == 1
	assert s.min_rotated_array([5,1,2,3,4]) == 1
	assert s.min_rotated_array([1,2,3,4,5]) == 1
	assert s.min_rotated_array([3,4,5,1,2]) == 1
	assert s.min_rotated_array([11,1,2,3,4,5,6,7,8,9,10]) == 1
	assert s.min_rotated_array([6,7,3,4,5]) == 3

