'''
http://www.geeksforgeeks.org/count-negative-numbers-in-a-column-wise-row-wise-sorted-matrix/

Find number of negatives in the matrix sorted row-wise and column-wise

Input:  
[-3, -2, -1,  1]
[-2,  2,  3,  4]
[4,   5,  7,  8]
Output : 4
'''

# Find number of negatives in a m x n sorted matrix
def find_negatives(matrix, m, n):
	# Start with last column in row 0
	i, j = 0, n-1

	count = 0

	while j >=0 and i < m:
		if (matrix[i][j] < 0):
			count += j+1

			# skip to next row
			i += 1
		else:
			# Move left until a negative number is found
			j -= 1

	return count


matrix = [
		  [-3, -2, -1,  1],
		  [-2,  2,  3,  4],
		  [4,   5,  7,  8]
		]
assert(find_negatives(matrix, 3, 4) == 4)
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
