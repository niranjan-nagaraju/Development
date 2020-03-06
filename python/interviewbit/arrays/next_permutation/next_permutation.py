'''
https://www.interviewbit.com/problems/next-permutation/

Next Permutation

Implement the next permutation, which rearranges numbers into the numerically next greater permutation of numbers for a given array A of size N.
If such arrangement is not possible, it must be rearranged as the lowest possible order i.e., sorted in an ascending order.

Note:
1. The replacement must be in-place, do **not** allocate extra memory.
2. DO NOT USE LIBRARY FUNCTION FOR NEXT PERMUTATION.
Use of Library functions will disqualify your submission retroactively and will give you penalty points.

Input Format:
	The first and the only argument of input has an array of integers, A.
Output Format:
	Return an array of integers, representing the next permutation of the given array.
Constraints:
	1 <= N <= 5e5
	1 <= A[i] <= 1e9


Examples:

Input 1:
    A = [1, 2, 3]
Output 1:
    [1, 3, 2]

Input 2:
    A = [3, 2, 1]
Output 2:
    [1, 2, 3]

Input 3:
    A = [1, 1, 5]
Output 3:
    [1, 5, 1]

Input 4:
    A = [20, 50, 113]
Output 4:
    [20, 113, 50]
'''

'''
Solution Outline:
	Consider a sequence x1, x2, ... xk, ..., xn
    Let xk ... xn be a non-increasing sequence (xk >= xk+1 >= ... >= xn)
	Then xk-1 < xk
     Find the successor to xk-1 in xk, ... xn (ie the smallest number in {xk.,,, xn} > xk-1), xs
     Swap xk-1 with xs (so xk-1 now is replaced with the next number immediately greater than it)
     Reverse the entire sequence {xk, ... xk-1, ... xn} so its non-decreasing
	Next permutation: x1, x2, .. xs, xn, ... xk-1, .... xk


Sample run:
   A: [1, 3, 5, 4, 2]
   Non-increasing sequence from the right: [5,4,2]
   Find successor to 3 in [5,4,2] == 4
   Swap [3] with [4] =>
     A: [1, 4, 5, 3, 2]
   Reverse the non-increasing sequence
    A: [1, 4, 2, 3, 5]
'''

class Solution:
	# @param A : list of integers
	# @return a list of integers
	def next_permutation(self, A):
		n = len(A)
		if n <= 1:
			return A

		# Find a non-decreasing sequence from the right
		i = n-1
		while i>0 and A[i] <= A[i-1]:
			i -= 1

		# Entire array is in decreasing order
		# There's no next permutation
		# return reverse of current array
		if i == 0:
			self.reverse(A, 0, n-1)
			return A

		x = A[i-1]
		nge_idx = self.find_next_greater_in_reverse_sorted(A, x, i, n-1)
		A[i-1], A[nge_idx] = A[nge_idx], x
		self.reverse(A, i, n-1)
		return A


	# find next greater number of 'key' in reverse-sorted 'lst'
	# and return its index
	def find_next_greater_in_reverse_sorted(self, lst, key, l=0, h=None):
		if h == None:
			h = len(lst)-1
		nge_idx = -1
		while l <= h:
			mid = (l+h)/2
			if lst[mid] > key:
				nge_idx = mid
				# Found a candidate for NGE
				# Look to the right so if we can find a better match
				l = mid+1
			else:
				# lst[mid] <= key
				# Look to the left
				h = mid-1

		return nge_idx



	# reverse array A[l..h] in-place
	def reverse(self, A, l, h):
		while l <= h:
			A[l], A[h] = A[h], A[l]
			l+=1
			h-=1



if __name__ == '__main__':
	s = Solution()
	assert s.next_permutation([]) == []
	assert s.next_permutation([5]) == [5]
	assert s.next_permutation([5,4,3,2,1])  == [1,2,3,4,5]
	assert s.next_permutation([1,3,5,4,2]) == [1,4,2,3,5]
	assert s.next_permutation([1, 2, 3]) == [1,3,2]
	assert s.next_permutation([3, 2, 1]) == [1,2,3]
	assert s.next_permutation([1, 1, 5]) == [1,5,1]
	assert s.next_permutation([20, 50, 113]) == [20, 113, 50]

