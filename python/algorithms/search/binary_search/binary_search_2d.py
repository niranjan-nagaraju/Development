'''
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

# Binary search linear version
def binary_search_2d(matrix, rows, columns, item):
	# Start with last column in row 0
	i, j = 0, columns-1

	while j >=0 and i < rows:
		x = matrix[i][j]

		# Start optimistic
		if x == item:
			return (i,j)
		elif (x < item):
			# skip current row
			i += 1
		else: # item > matrix[i][j]
			# skip current column
			j -= 1

	return (None, None)

	

# Binary search recursive version
# NOTE: since we start at top row, and skip rows as needed, actual row would always be 0 while returning a match,
#		we keep track of the number of rows skipped so far to return the actual row in the matrix where we found a match
# NOTE: The skip-count is not needed for columns as the columns start from the end, and therefore are at the right column
#		when we match
def binary_search_2d_r(matrix, rows, columns, item, skipped_rows=0):
	#print "Processing matrix %d x %d:" %(rows, columns), matrix

	# Exhausted all rows or columns and we haven't found the item in the matrix
	if rows == 0 or columns == 0:
		return (None, None)

	x = matrix[0][columns-1]
	
	# Start optimistic
	if x == item:
		return (skipped_rows, columns-1)
	elif  x < item:
		# skip current row
		return binary_search_2d_r(matrix[1:], rows-1, columns, item, skipped_rows+1)
	else: # item > matrix[i][j]
		# skip current column
		return binary_search_2d_r(map((lambda m2d: m2d[:-1]), matrix), rows, columns-1, item, skipped_rows)



# Binary search recursive version (optimized to avoid copying of matrix contents on each invocation
def binary_search_2d_r2(matrix, rows, columns, item, skiprows=0, skipcolumns=0):
	# Exhausted all rows or columns and we haven't found the item in the matrix
	if rows == skiprows or columns == skipcolumns:
		return (None, None)

	x = matrix[skiprows][columns-skipcolumns-1]
	
	# Start optimistic
	if x == item:
		return (skiprows, columns-skipcolumns-1)
	elif  x < item:
		# skip current row
		return binary_search_2d_r2(matrix, rows, columns, item, skiprows+1, skipcolumns)
	else: # item > matrix[i][j]
		# skip current column
		return binary_search_2d_r2(matrix, rows, columns, item, skiprows, skipcolumns+1)



# Test a specified binary search implementation
def test_binary_search(f):
	assert( f (
		[
		  [1, 2,  3,  9],
		  [5, 6,  8,  10],
		  [7, 12, 13, 14]
		], 3, 4, 6) == (1,1))

	assert( f (
		[
		  [1, 2,  3,  9],
		  [5, 6,  8,  10],
		  [7, 12, 13, 14]
		], 3, 4, 4) == (None,None))


	assert( f (
		[
		  [1, 2,  3,  9],
		  [5, 6,  8,  10],
		  [7, 12, 13, 14]
		], 3, 4, 0) == (None,None))


	assert( f (
		[
		  [1, 2,  3,  9],
		  [5, 6,  8,  10],
		  [7, 12, 13, 14]
		], 3, 4, 15) == (None,None))

	assert( f (
		[
		  [1, 2,  3,  9],
		  [5, 6,  8,  10],
		  [7, 12, 13, 14]
		], 3, 4, 14) == (2,3))

	assert( f (
		[
		  [1, 2,  3,  9],
		  [5, 6,  8,  10],
		  [7, 12, 13, 14]
		], 3, 4, 13) == (2,2))

	assert( f (
		[
		  [1, 2,  3,  9],
		  [5, 6,  8,  10],
		  [7, 12, 13, 14]
		], 3, 4, 1) == (0,0))

	assert( f (
		[
		  [1, 3, 5, 7],
		  [2, 4, 6, 8],
		  [9, 11, 13, 15],
		  [10, 12, 14, 16]
		], 4, 4, 12) == (3,1))




if __name__ == '__main__':
	test_binary_search(binary_search_2d)
	test_binary_search(binary_search_2d_r)
	test_binary_search(binary_search_2d_r2)

