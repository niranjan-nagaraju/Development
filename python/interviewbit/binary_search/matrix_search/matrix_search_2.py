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
Solution Outline: O(log m + log n)
	1. Identify which row contains the key
	    Start with row[0], row[n-1]
		  mid = (0+n-1)/2
		  if A[mid][0] > key => skip rows below mid, [mid:]
		  if A[mid][0] < key, check if A[mid][-1] > key
		    If so return current row, mid
			else A[mid][-1] < key, => current row [0:m-1] are all < key,
			  skip all rows above mid, [:mid]
	2. Run binary search on that row for the key
'''
class Solution:
	def matrix_search(self, A, B):
		def find_row():
			l, h = 0, n-1
			while l <= h:
				mid = (l+h)/2
				if A[mid][0] == B:
					return mid

				if A[mid][0] > B:
					# Current row start > key
					# Look for rows above
					h = mid-1
				else:
					# Current row start < key
					if A[mid][-1] >= B:
						# row end >= key
						# key might be in current row
						return mid

					# Look for rows below
					l = mid+1
			
			# Couldn't find any row that might contain key
			return -1


		# Search for key in A[row][0:m-1]
		def bin_search(A, row, key):
			l, h = 0, m-1
			while l <= h:
				mid = (l+h)/2

				if A[row][mid] == key:
					return 1

				if A[row][mid] < key:
					l = mid+1
				else:
					h = mid-1

			return 0

		if not A or not A[0]:
			return 0

		n = len(A)
		m = len(A[0])

		row = find_row()
		if row < 0:
			return 0

		# Found key in the matching row's element
		if A[row][0] == B:
			return 1

		return bin_search(A, row, B)


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

