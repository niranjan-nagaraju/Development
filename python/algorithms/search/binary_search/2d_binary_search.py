'''
PLACEHOLDER:
	Given a M x N column-wise, and row-wise sorted matrix of numbers, Find a number in the matrix

	e.g.
	[
	  [1, 3, 5, 7],
	  [2, 4, 6, 8],
	  [9, 11, 13, 15],
	  [10, 12, 14, 16]
	]


	Solution:
		Start at top-row, last column (from here, everything to the left is < and everything down is >)
		x = matrix[i][j], i: 0, j: last column

		1. check if x matches element, we are done if it is.
		2. if x < element, we can skip current row, and start looking for element in matrix[i+1:][:]
		3. if x > element, we can skip current column, and start looking for element in matrix[:][j-1]
'''
