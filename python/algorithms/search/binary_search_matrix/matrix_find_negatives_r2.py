'''
http://www.geeksforgeeks.org/count-negative-numbers-in-a-column-wise-row-wise-sorted-matrix/

Find number of negatives in the matrix sorted row-wise and column-wise

Input:  
[-3, -2, -1,  1]
[-2,  2,  3,  4]
[4,   5,  7,  8]
Output : 4


Solution: (recursive version but avoids excessive copying of 2D array contents back and forth for every function call)
	Start with the full matrix, rightmost column, row 0,
	if the number at that index, is -ve, 
		=> everything to the left is -ve
		Add column number to count, recursively work on the rest of the matrix sans current row, and every row above
	else, 
		skip current column, recursively work on the rest of the matrix sans current column, and every column to the right
'''


def find_negatives(matrix, rows, columns, skiprows=0, skipcolumns=0):
	if rows == skiprows or columns == skipcolumns:
		return 0

	if matrix[skiprows][columns-skipcolumns-1] < 0:
		'''
		Found a -ve element
		Add column number to count
		skip current row

		e.g.
		[
		  [-1, -2, -3, -4],
		  [w, x, y, z],
		  . . .
		]
		=> 4 + find_negatives(
				[
				  [w, x, y, z],
				  . . .
				])
		'''
		return (columns-skipcolumns) + find_negatives(matrix, rows, columns, skiprows+1, skipcolumns)
	else:
		'''
		Found a non-negative element
		skip current column

		e.g.
		[
		  [1, 2, 3, 4],
		  [w, x, y, z],
		  . . .
		  [a, b, c, d]
		]
		=> find_negatives(
				[
				  [1, 2, 3],
				  [w, x, y],
				  . . .
				  [a, b, c]
				])


		Lambda to skip last column in a matrix
		>>> m
		[[1, 3, 5, 7], [2, 4, 6, 8], [9, 11, 13, 15], [10, 12, 14, 16]]

		>>> map((lambda m: m[:-1]), m)
		[[1, 3, 5], [2, 4, 6], [9, 11, 13], [10, 12, 14]]
		'''
		return find_negatives(matrix, rows, columns, skiprows, skipcolumns+1)




if __name__ == '__main__':
	matrix = [
			  [-3, -2, -1,  1],
			  [-2,  2,  3,  4],
			  [4,   5,  7,  8]
			]
	assert(find_negatives(matrix, 3, 4) == 4)

	assert(find_negatives([[-1, -2, -3, -4]], 1, 4) == 4)

	assert(find_negatives(
		[
			[-7, -6, -5, -4],
			[-6, -5, -4, -3],
			[-5, -4, -3, -2],
			[-4, -3, -2, -1]
		], 
		4, 4) == 16)

	assert(find_negatives(
		[
			[1, 2, 3, 4],
			[2, 3, 4, 5],
			[3, 4, 5, 6],
			[4, 5, 6, 7]
		], 
		4, 4) == 0)

	assert(find_negatives(
		[
			[-4, 2, 3, 4],
			[-3, 3, 4, 5],
			[-2, 4, 5, 6],
			[-1, 5, 6, 7]
		], 
		4, 4) == 4)

	assert(find_negatives(
		[
			[1, 2, 3],
			[2, 3, 4],
			[4, 5, 6]
		], 
		3, 3) == 0)

