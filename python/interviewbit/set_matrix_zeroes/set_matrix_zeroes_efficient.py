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
  Memory: O(1), Time: O(mn)
  Use the top row and left column as 'headers' that indicate if their respective column/row contains 0s
  Then traverse the rest of the matrix and check if the 'header' says this row/column needs to be 0 and set accordingly.
  NOTE: since the original top row/left column is lost, use a single flag for each of row1/col1 to indicate if they need to be set to 0s.


Sample run:
	M:
	  [
	    [1, 1, 1, 0],
		[1, 0, 1, 1],
		[1, 1, 1, 1],
		[1, 0, 1, 1]
	  ]
	  row1 contains 0 => zeroRowFlag = 1
	  col1 has no 0s => zeroColFlag = 0

	  Traverse the inner matrix, and use row1/col1 as headers to store 0s if the row/col needs to be set to 0s

	  Sub-Matrix:
	  [
	    [0, 1, 1],
	    [1, 1, 1],
	    [0, 1, 1]
	  ]

	  Row1: [0, 1, 1]
	  Update matrix
	  [
	    [1, 0, 1, 0],
		[0, 0, 1, 1],
		[1, 1, 1, 1],
		[1, 0, 1, 1]
	  ]

	  Row2: [1, 1, 1]
	  Update matrix
	  [
	    [1, 0, 1, 0],
		[0, 0, 1, 1],
		[1, 1, 1, 1],
		[1, 0, 1, 1]
	  ]

	  Row3: [0,1,1]
	  Update matrix
	  [
	    [1, 0, 1, 0],
		[0, 0, 1, 1],
		[1, 1, 1, 1],
		[0, 0, 1, 1]
	  ]

	  Now scan the inner-matrix, and set the column/row to 0 if their headers contain 0s
	  Update matrix
	  [
	    [1, 0, 1, 0],
		[0, 0, 0, 0],
		[1, 0, 1, 0],
		[0, 0, 0, 0]
	  ]
	  row1 had a zero, => set row 0 to all zeroes
	  col1 had no zeroes, dont change col1
	  Update matrix
	  [
	    [0, 0, 0, 0],
		[0, 0, 0, 0],
		[1, 0, 1, 0],
		[0, 0, 0, 0]
	  ]
'''
class Solution:
	# @param A : list of list of integers
	# @return the same list modified
	def setZeroes(self, A):
		rows = len(A)
		cols = len(A[0])

		zeroInRow1 = False
		zeroInCol1 = False
		for c in xrange(cols):
			if A[0][c] == 0:
				zeroInRow1 = True

		for r in xrange(rows):
			if A[r][0] == 0:
				zeroInCol1 = True


		# Set outer 'headers' to 0s based on if the inner matrix
		# contains 0s
		for r in xrange(1, rows):
			for c in xrange(1, cols):
				if A[r][c] == 0:
					A[0][c] = 0
					A[r][0] = 0

		# Traverse the matrix, set entire column or row to 0
		# if their row/column header is 0
		# i.e A[i][j] = 0 if A[0][j] = 0 or A[i][0] == 0
		for r in xrange(1, rows):
			for c in xrange(1, cols):
				if A[0][c] == 0 or A[r][0] == 0:
					A[r][c] = 0
		
		# Update top-row/left-column to 0s if they originally had a 0
		for c in xrange(cols):
			if zeroInRow1:
				A[0][c] = 0

		for r in xrange(rows):
			if zeroInCol1:
				A[r][0] = 0

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

