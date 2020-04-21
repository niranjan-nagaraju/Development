'''
https://www.interviewbit.com/problems/matrix-search/

Matrix Search

Given a matrix of integers A of size N x M and an integer B.

Write an efficient algorithm that searches for integar B in matrix A.

This matrix A has the following properties:

Integers in each row are sorted from left to right.
The first integer of each row is greater than or equal to the last integer of the previous row.
Return 1 if B is present in A, else return 0.

Note: Rows are numbered from top to bottom and columns are numbered from left to right.



Input Format
The first argument given is the integer matrix A.
The second argument given is the integer B.
Output Format

Return 1 if B is present in A, else return 0.

Constraints
1 <= N, M <= 1000
1 <= A[i][j], B <= 10^6

For Example
Input 1:
    A = 
    [ [1,   3,  5,  7],
      [10, 11, 16, 20],
      [23, 30, 34, 50]  ]
    B = 3
Output 1:
    1

Input 2:
    A = [   [5, 17, 100, 111]
            [119, 120,  127,   131]    ]
    B = 3
Output 2:
    0
'''

'''
Solution Outline:
	1. Regular binary search works by eliminating half the array at each iteration.
	2. Start with x == (top-row, right-column), so that all elements to the left are decreasing, all elements below are increasing
	   Check if x == key, return true
	   If x < key, skip entire row
	   if x > key: skip entire column
'''
class Solution:
	def matrix_search(self, A, B):
		if not A or not A[0]:
			return 0
		n = len(A)
		m = len(A[0])

		i, j = 0, m-1
		while i<n and j>=0:
			x = A[i][j]
			if x == B:
				return 1

			if x < B:
				# Entire row 'i' can be skipped
				# A[i][j] is the highest in its row, i
				i += 1
			else: 
				# x > B => entire column 'j' can be skipped
				# A[i][j] is the lowest in its column, j
				j -= 1

		# Couldn't locate key 'B' in A
		return 0


if __name__ == '__main__':
	s = Solution()
	assert s.matrix_search(B=3, A = [
		[1,   3,  5,  7],
		[10, 11, 16, 20],
		[23, 30, 34, 50]  ]) == 1

	assert s.matrix_search(B=23, A = [
		[1,   3,  5,  7],
		[10, 11, 16, 20],
		[23, 30, 34, 50]  ]) == 1

	assert s.matrix_search(B=21, A = [
		[1,   3,  5,  7],
		[10, 11, 16, 20],
		[23, 30, 34, 50]  ]) == 0

	assert s.matrix_search(B=3, A =[
		[5, 17, 100, 111],
		[119, 120,  127, 131] ]) == 0

