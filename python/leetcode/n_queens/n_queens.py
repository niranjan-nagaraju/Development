#+encoding: utf-8

'''
https://leetcode.com/problems/n-queens/

The n-queens puzzle is the problem of placing n queens on an n×n chessboard such that no two queens attack each other.

Given an integer n, return all distinct solutions to the n-queens puzzle.

Each solution contains a distinct board configuration of the n-queens' placement, where 'Q' and '.' both indicate a queen and an empty space respectively.

Example:
Input: 4
Output: [
 [".Q..",  // Solution 1
  "...Q",
  "Q...",
  "..Q."],

 ["..Q.",  // Solution 2
  "Q...",
  "...Q",
  ".Q.."]
]
Explanation: There exist two distinct solutions to the 4-queens puzzle as shown above.
'''

class Solution(object):
	def solveNQueens(self, n):
		"""
		:type n: int
		:rtype: List[List[str]]
		"""

		# Prints a nxn board with the positions of
		# queens placed indicated as Q, and empty squares as .
		def print_board_to_solution(board):
			sol = []
			for i in xrange(n):
				sol.append('.' * board[i] + 'Q' + '.' * (n - board[i] - 1))
			solutions.append(sol)


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
				print_board_to_solution(board)
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

		return solutions


if __name__ == '__main__':
	s = Solution()
	assert s.solveNQueens(1) == [["Q"]]
	assert s.solveNQueens(2) == []
	assert s.solveNQueens(3) == []
	assert s.solveNQueens(4) == [
			[".Q..",  
			 "...Q",
			 "Q...",
			 "..Q."],
			["..Q.",  
			 "Q...",
			 "...Q",
			 ".Q.."]
			]

