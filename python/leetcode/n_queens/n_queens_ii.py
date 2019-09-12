#+encoding: utf-8

'''
https://leetcode.com/problems/n-queens_ii/

The n-queens puzzle is the problem of placing n queens on an n×n chessboard such that no two queens attack each other.

Given an integer n, return the number of distinct solutions to the n-queens puzzle.

Example:
Input: 4
Output: 2
Explanation: There exist two distinct solutions to the 4-queens puzzle as shown below.
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

class Solution(object):
	def totalNQueens(self, n):
		"""
		:type n: int
		:rtype: int
		"""

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
				solutions[0] += 1
				return

			for c in xrange(n):
				if safe(q, c):
					board[q] = c
					place(q+1)

					# backtrack
					board[q] = None


		# Empty board to begin with
		board = [None]*n
		solutions = [0]

		# Start with placing Queen 0
		place(0)

		return solutions[0]


if __name__ == '__main__':
	s = Solution()
	assert s.totalNQueens(1) == 1
	assert s.totalNQueens(2) == 0
	assert s.totalNQueens(3) == 0
	assert s.totalNQueens(4) == 2
	assert s.totalNQueens(5) == 10

