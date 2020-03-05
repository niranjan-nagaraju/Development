'''
https://www.interviewbit.com/problems/set-matrix-zeros/

Set Matrix Zeros

Given a matrix, A of size M x N of 0s and 1s. If an element is 0, set its entire row and column to 0.

Note: This will be evaluated on the extra memory used. Try to minimize the space and time complexity.

Input Format:
The first and the only argument of input contains a 2-d integer matrix, A, of size M x N.

Output Format:
Return a 2-d matrix that satisfies the given conditions.

Constraints:
1 <= N, M <= 1000
0 <= A[i][j] <= 1

Examples:
Input 1:
    [   [1, 0, 1],
        [1, 1, 1], 
        [1, 1, 1]   ]

Output 1:
    [   [0, 0, 0],
        [1, 0, 1],
        [1, 0, 1]   ]

Input 2:
    [   [1, 0, 1],
        [1, 1, 1],
        [1, 0, 1]   ]

Output 2:
    [   [0, 0, 0],
        [1, 0, 1],
        [0, 0, 0]   ]
'''

'''
Solution Outline:
  Memory: O(1), Time: O(mn*(m+n))
	Traverse the matrix row-wise, and replace M[i][j] = -1 if M[i][j] = -1
	Scan the matrix again, and if M[i][j] = -1, set all M[x][j] 0<=x<m, and M[i][y], 0<=y<n to 0
	   unless any of M[x][j]/M[i][y]  {x!=i, y!=j} itself is -1
	 We keep the other -1s in the row/column as-is to remember they originally had 0 there and we have to 
	 set their rows/columns to 0s.
'''

class Solution:
	# @param A : list of list of integers
	# @return the same list modified
	def setZeroes(self, A):
		def clear_columns(col):
			for j in xrange(m):
				if A[j][col] != -1:
					A[j][col] = 0

		def clear_rows(row):
			for j in xrange(n):
				if A[row][j] != -1:
					A[row][j] = 0


		m = len(A)
		n = len(A[0])
		for i in xrange(m):
			for j in xrange(n):
				if A[i][j] == 0:
					A[i][j] = -1

		for i in xrange(m):
			for j in xrange(n):
				if A[i][j] == -1:
					A[i][j] = 0
					clear_rows(i)
					clear_columns(j)
		return A


if __name__ == '__main__':
	s = Solution()
	m1 =     [  [1, 0, 1],
				[1, 1, 1], 
				[1, 1, 1]   ]
	assert s.setZeroes(m1) == [[0, 0, 0], [1, 0, 1], [1, 0, 1]]

	m2 =  [ [1, 0, 1],
			[1, 1, 1],
			[1, 0, 1]   ]
	assert s.setZeroes(m2) == [[0, 0, 0], [1, 0, 1], [0, 0, 0]]

	m3 = [ [1, 0, 1],
		   [0, 0, 1],
		   [1, 1, 1]]
	assert s.setZeroes(m3) == [[0, 0, 0], [0, 0, 0], [0, 0, 1]]

