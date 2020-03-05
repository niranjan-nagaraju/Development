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
  Memory: O(m+n), Time: O(mn)
	Use two arrays, one for storing which rows contain 0s
	and another for storing which columns contain 0s

	set zero_rows[i] = 1 if M[i][j] = 0
	set zero_cols[j] = 1 if M[i][j] = 0

	Traverse the matrix row-wise, and set M[i][j] = 0 if zero_rows[i] = 0 or zero_cols[j] = 0
'''
class Solution:
	# @param A : list of list of integers
	# @return the same list modified
	def setZeroes(self, A):
		m = len(A)
		n = len(A[0])
		zero_rows = [0]*m
		zero_cols = [0]*n
		for i in xrange(m):
			for j in xrange(n):
				if A[i][j] == 0:
					zero_rows[i] = 1
					zero_cols[j] = 1

		for i in xrange(m):
			for j in xrange(n):
				if zero_rows[i] == 1 or zero_cols[j] == 1:
					A[i][j] = 0
			
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
	m4 = [
			[1, 1, 1, 0],
			[1, 0, 1, 1],
			[1, 1, 1, 1],
			[1, 0, 1, 1]
		 ]
	assert s.setZeroes(m4) == [
			[0, 0, 0, 0],
			[0, 0, 0, 0],
			[1, 0, 1, 0],
			[0, 0, 0, 0]
		 ]

	m5 = [ [0, 1, 1, 0], 
		   [1, 1, 0, 1], 
		   [1, 1, 1, 1]] 
	assert s.setZeroes(m5) == [
			[0, 0, 0, 0], 
		    [0, 0, 0, 0], 
		    [0, 1, 0, 0]]

