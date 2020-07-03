'''
https://www.interviewbit.com/problems/nqueens/

NQueens

The n-queens puzzle is the problem of placing n queens on an nxn chessboard such that no two queens attack each other.

Given an integer n, return all distinct solutions to the n-queens puzzle.

Each solution contains a distinct board configuration of the n-queens' placement, where 'Q' and '.' both indicate a queen and an empty space respectively.

For example,
There exist two distinct solutions to the 4-queens puzzle:

[
 [".Q..",  // Solution 1
  "...Q",
  "Q...",
  "..Q."],

 ["..Q.",  // Solution 2
  "Q...",
  "...Q",
  ".Q.."]
]
'''

'''
Solution Outline:
	1. Use a 1-D table of 'n' to store n queens' positions on the board
		Q0 will always be in row0, Q1 will be in row1, etc, so we don't have to use a nxn grid.
		for e.g, n: 4
		Queens: [0, 2, 3, 1] is a configuration that corresponds to the following board (nevermind if they are a valid solution) -
		Q . . .
		. . Q .
		. . . Q
		. Q . .
	2. Start with finding a safe place for Q0 on row 0,
		find a safe place for Q1 on row 1, 
			....
			find a safe place for Qi on row i,
			  . . .
			If we are able to find safe places for all n queens, we have a valid solution -> add to solution set.
			Otherwise, If we are not able to find a safe-place for Queen Qi on row i,
				backtrack to Qi-1, Try to find a safe place to the right of its current position, ... if not backtrack to Qi-2, ...

Sample run:
	n: 4
	
	Q: []

	place (Q0, 0)
	Q: [0]
	Q . . .
	. . . .
	. . . .
	. . . .
		place (Q1, 0) -> conflict
		place (Q1, 1) -> conflict
		place (Q1, 2)
		Q: [0, 2]
		Q . . .
		. . Q .
		. . . .
		. . . .
			place(Q2, 0) -> conflict
			place(Q2, 1) -> conflict
			place(Q2, 2) -> conflict
			place(Q2, 3) -> conflict
		backtrack
		place (Q1, 3)
		Q . . .
		. . . Q
		. . . .
		. . . .
			place(Q2, 0) -> conflict
			place(Q2, 1)
			Q . . .
			. . . Q
			. Q . .
			. . . .
				place(Q3, 0) -> conflict
				place(Q3, 1) -> conflict
				place(Q3, 2) -> conflict
				place(Q3, 3) -> conflict
			backtrack
			place(Q2, 2) -> conflict
			place(Q2, 3) -> conflict
		backtrack
		place (Q1, 4) ? Out of bounds
	backtrack
	place (Q0, 1)
	. Q . .
	. . . .
	. . . .
	. . . .
		place (Q1, 0) -> conflict
		place (Q1, 1) -> conflict
		place (Q1, 2) -> conflict
		place (Q1, 3)
		. Q . .
		. . . Q
		. . . .
		. . . .
			place (Q2, 0)
			. Q . .
			. . . Q
			Q . . .
			. . . .
				place (Q3, 0) -> conflict
				place (Q3, 1) -> conflict
				place (Q3, 2)
				. Q . .
				. . . Q
				Q . . .
				. . Q .
					{**Add to Solution**}
				backtrack to find all solutions
				place (Q3, 3) -> conflict
			backtrack
			place (Q2, 1) -> conflict
			place (Q2, 2) -> conflict
			place (Q2, 3) -> conflict
		backtrack
		place (Q1, 4)  ? Out of bounds
	backtrack
	place (Q0, 2)
	. . Q .
	. . . .
	. . . .
	. . . .
		place (Q1, 0)
		. . Q .
		Q . . .
		. . . .
		. . . .
			place (Q2, 0) -> conflict
			place (Q2, 1) -> conflict
			place (Q2, 2) -> conflict
			place (Q2, 3)
			. . Q .
			Q . . .
			. . . Q
			. . . .
				place (Q3, 0) -> conflict
				place (Q3, 1)
				. . Q .
				Q . . .
				. . . Q
				. Q . .
					{**Add to Solution**}
				backtrack to find all solutions
				place (Q3, 2) -> conflict
				place (Q3, 3) -> conflict
			backtrack
			place (Q2, 4) ? Out of bounds
		backtrack
		place (Q1, 1) -> conflict
		place (Q1, 2) -> conflict
		place (Q1, 3) -> conflict
	backtrack
	place (Q0, 3)
	. . . Q
	. . . .
	. . . .
	. . . .
		place (Q1, 0)
		. . . Q
		Q . . .
		. . . .
		. . . .
			place (Q2, 0) -> conflict
			place (Q2, 1) -> conflict
			place (Q2, 2)
			. . . Q
			Q . . .
			. . Q .
			. . . .
				place (Q3, 0) -> conflict
				place (Q3, 1) -> conflict
				place (Q3, 2) -> conflict
				place (Q3, 3) -> conflict
			backtrack
			place (Q2, 3) -> conflict
		backtrack
		place (Q1, 1)
		. . . Q
		. Q . .
		. . . .
		. . . .
			place (Q2, 0) -> conflict
			place (Q2, 1) -> conflict
			place (Q2, 2) -> conflict
			place (Q2, 3) -> conflict
		backtrack
		place (Q1, 2) -> conflict
		place (Q1, 3) -> conflict
	backtrack
	place (Q0, 4) ? Out of bounds
		  

Solutions:
. Q . .
. . . Q
Q . . .
. . Q .

. . Q .
Q . . .
. . . Q
. Q . .

'''
class Solution:
	def place_n_queens(self, n):
		# return if queen 'q' can be safely
		# placed in row 'q', column 'c'
		# Queens 0..q-1 are already placed
		def is_safe(q, c):
			for x in xrange(q):
				row,col = x,board[x]
				# There's already a queen 'x' at column c
				if col == c:
					return False
				# There's a queen on the same diagonal (secondary direction)
				if row+col == q+c:
					return False
				if col-row == c-q:
					return False
			return True

		
		# Add current board configuration to solution
		def add_board_to_solution():
			sol = []
			for x in board:
				sol.append('.'*x + 'Q' + '.'*(n-1-x))
			solution_boards.append(sol)


		# place queen 'q' on the board, and recursively try to place queens q+1..n
		def place(q):
			# All 'n' queens have been placed
			# without conflicts on the board
			if q == n:
				add_board_to_solution()

			for i in xrange(n):
				if is_safe(q, i):
					board[q] = i
					place(q+1)
					board[q] = None # backtrack

		if n == 0:
			return []

		board = [None]*n
		solution_boards = []

		# place queen 0 on the board
		place(0)

		return solution_boards


if __name__ == '__main__':
	s = Solution()
	assert s.place_n_queens(1) == [["Q"]]
	assert s.place_n_queens(2) == []
	assert s.place_n_queens(3) == []
	assert s.place_n_queens(4) == [
			[".Q..",
			 "...Q",
			 "Q...",
			 "..Q."],
			["..Q.",
			 "Q...",
			 "...Q",
			 ".Q.."]]
	assert len(s.place_n_queens(5)) == 10
	assert len(s.place_n_queens(8)) == 92

