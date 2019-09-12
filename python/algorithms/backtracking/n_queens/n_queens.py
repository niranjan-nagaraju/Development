'''
Place n queens in a chessboard of nxn squares such that no two queens are in an attacking position wrt each other.
Print all board positions possible.
'''

'''
Solution Outline:
	Each queen, Qi, has to be placed in a separate row of their own (also applies to column).
	Start with placing Q0 in column 0 (=> 0,0), then try to place the remaining (n-1) queens in their respective rows.
	If any queen Qi cannot be placed in its entire row,i, backtrack to Qi-1, and move over to the right and start over.


Sample run:
	n = 4
	Q: _ _ _ _

	i: 0
	  Q0 at row 0, column 0

	  Q: 0 _ _ _
	  Chessboard:
	  Q _ _ _ [Q3 couldnt be placed]
	  _ _ _ _
	  _ _ _ _
	  _ _ _ _

		  place Q1 in row 1
		  Q: 0 2 _ _
		  Chessboard:
		  Q _ _ _
		  _ _ Q _
		  _ _ _ _
		  _ _ _ _

			  place Q2 in row 2
			  Chessboard:
			  Q _ _ _
			  _ _ Q _
			  _ _ _ _  [Cant place, backtrack]
			  _ _ _ _

		  place Q1 in row 1
		  Q: 0 3 _ _
		  Chessboard:
		  Q _ _ _
		  _ _ _ Q [Q3, couldnt be placed, backtrack]
		  _ _ _ _
		  _ _ _ _
			  place Q2 in row 2
			  Q: 0 3 1 _
			  Chessboard:
			  Q _ _ _
			  _ _ _ Q   
			  _ Q _ _  [Q3 couldnt be placed, backtrack]
			  _ _ _ _
				  place Q3 in row 3
				  Q: 0 3 1 _
				  Chessboard:
				  Q _ _ _
				  _ _ _ Q
				  _ Q _ _  
				  _ _ _ _ [Cant place, backtrack]

	i: 1
	  Q0 at row 0, column 1

	  Q: 1 _ _ _
	  Chessboard:
	  _ Q _ _ 
	  _ _ _ _
	  _ _ _ _
	  _ _ _ _
		  place Q1 in row 1
		  Q: 1 3 _ _
		  Chessboard:
		  _ Q _ _
		  _ _ _ Q   
		  _ _ _ _  
		  _ _ _ _
			  place Q2 in row 2
			  Q: 1 3 0 _
			  Chessboard:
			  _ Q _ _
			  _ _ _ Q   
			  Q _ _ _  
			  _ _ _ _
				  place Q3 in row 3
				  Q: 1 3 0 2
				  Chessboard:
				  _ Q _ _
				  _ _ _ Q   
				  Q _ _ _  
				  _ _ Q _ ===> [Solution #1]
			  place Q2 in row 2, after index 0 [cant place]
		  place Q1 in row 1, after index 3 [cant place]

	i: 2
	  Q0 at row 0, column 2

	  Q: 2 _ _ _
	  Chessboard:
	  _ _ Q _ 
	  _ _ _ _
	  _ _ _ _
	  _ _ _ _
		  place Q1 in row 1
		  Q: 2 0 _ _
		  Chessboard:
		  _ _ Q _ 
		  Q _ _ _
		  _ _ _ _
		  _ _ _ _
			  place Q2 in row 2
			  Q: 2 0 3 _
			  Chessboard:
			  _ _ Q _ 
			  Q _ _ _
			  _ _ _ Q
			  _ _ _ _
				  place Q3 in row 3
				  Q: 2 0 3 1
				  Chessboard:
				  _ _ Q _ 
				  Q _ _ _
				  _ _ _ Q
				  _ Q _ _  => [Solution #2]
				  place Q3 in row 3 to right of column 1 [Cant place]
			  place Q2 in row 2 to right of 3 [cant place]
		  place Q1 in row 1 to right of index 0 [cant place]

	i: 3
	  Q0 at row 0, column 2

	  Q: 3 _ _ _
	  Chessboard:
	  _ _ _ Q
	  _ _ _ _
	  _ _ _ _
	  _ _ _ _
		  place Q1 in row 1
		  Q: 3 0 _ _
		  Chessboard:
		  _ _ _ Q 
		  Q _ _ _
		  _ _ _ _
		  _ _ _ _
			  place Q2 in row 2
			  Q: 3 0 2 _
			  Chessboard:
			  _ _ _ Q 
			  Q _ _ _
			  _ _ Q _
			  _ _ _ _
				  place Q3 in row 3
				  Q: 3 0 2 _
				  Chessboard:
				  _ _ _ Q 
				  Q _ _ _
				  _ _ Q _
				  _ _ _ _ [cant place]
			  place Q2 in row 2, right of column 2 [cant place]
		  place Q1 in row 1, right of column 0
		  Q: 3 1 _ _
		  Chessboard:
		  _ _ _ Q 
		  _ Q _ _
		  _ _ _ _
		  _ _ _ _
			  place Q2 in row 2
			  Q: 3 1 _ _
			  Chessboard:
			  _ _ _ Q 
			  _ Q _ _
			  _ _ _ _  [cant place]
			  _ _ _ _
		  place Q1 in row 1, right of column 1 [cant place]
	  place Q0 in row 0, right of column 3 [cant place]

	i: 4 [END]
	Two Solutions Found
	  _ Q _ _
	  _ _ _ Q   
	  Q _ _ _  
	  _ _ Q _ 

	  _ _ Q _ 
	  Q _ _ _
	  _ _ _ Q
	  _ Q _ _ 


Check for safe positions
  (r1, c1) vs (r2, c2)

  r1 == r2 [unsafe]
  c1 == c2 [unsafe]
  primary diagonal:
    (c1-r1) == (c2-r2)  [e.g., (1,2) vs (2,3), (0,1) vs (1,2)]  [unsafe]
  secondary diagonal:
    (c1+r1) == (c2+r2)  [e.g., (0,2) vs (1,1), (0,3) vs (1,2)]  [unsafe]
'''


def n_queens(n, toprint=False):
	# Prints a nxn board with the positions of
	# queens placed indicated as Q, and empty squares as _
	def print_board(board):
		print
		for i in xrange(n):
			print '_ ' * board[i] + 'Q' + ' _' * (n - board[i] - 1)


	# Check if column c is safe for Queen r,
	# given that all queens 0..r-1 are already placed
	def safe(r, c):
		for i in xrange(r):
			r1, c1 = i, board[i]
			if (r == r1) or (c == c1) or ((c-r) == (c1-r1)) or ((c+r) == (c1+r1)):
				return False
		return True


	# Recursively place q, .. n-1
	def place(q):
		# Placed all n queens so far
		if q == n:
			solutions.append(board[:])
			return

		for c in xrange(n):
			if safe(q, c):
				board[q] = c
				place(q+1)

				# backtrack
				board[q] = None


	# Empty board to begin with
	board = [None]*n
	solutions = []

	# Start with placing Queen 0
	place(0)

	# Do not print board unless explicity asked to
	if toprint:
		for s in solutions:
			# print an nxn board with the solution
			print_board(s)

	return solutions



if __name__ == '__main__':
	assert n_queens(1) == [[0]]
	assert n_queens(2) == []
	assert n_queens(3) == []
	assert n_queens(4, toprint=True) == [[1,3,0,2], [2,0,3,1]]
	assert len(n_queens(5)) == 10
	assert len(n_queens(8)) == 92

