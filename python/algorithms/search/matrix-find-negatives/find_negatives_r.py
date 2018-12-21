'''
http://www.geeksforgeeks.org/count-negative-numbers-in-a-column-wise-row-wise-sorted-matrix/

Find number of negatives in the matrix sorted row-wise and column-wise

Input:  
[-3, -2, -1,  1]
[-2,  2,  3,  4]
[4,   5,  7,  8]
Output : 4


Solution: (recursive version)
	Start with the full matrix, rightmost column, row 0,
	if the number at that index, is -ve, 
		=> everything to the left is -ve
		Update count to (column + 1), recursively work on the rest of the matrix sans current row, and every row above
	else, 
		skip current column, recursively work on the rest of the matrix sans current column, and every column to the right
'''


def find_negatives(matrix, rows, columns):
	#print "Processing matrix %d x %d:" %(rows, columns), matrix

	if rows == 0 or columns == 0:
		return 0

	if matrix[0][columns-1] < 0:
		'''
		Found a -ve element
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
		#print "Found -ve, Count in this iteration: ", columns
		return columns + find_negatives(matrix[1:], rows-1, columns)
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
		#print "Found non-negative"
		return find_negatives(map((lambda m2d: m2d[:-1]), matrix), rows, columns-1)




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

