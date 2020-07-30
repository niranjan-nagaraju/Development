#encoding: utf-8
'''
https://www.interviewbit.com/problems/valid-sudoku/

Determine if a Sudoku is valid, according to: http://sudoku.com.au/TheRules.aspx
The Sudoku board could be partially filled, where empty cells are filled with the character ‘.’.

Example:
["53..7....", "6..195...", ".98....6.", "8...6...3", "4..8.3..1", "7...2...6", ".6....28.", "...419..5", "....8..79"]
A partially filled sudoku which is valid.

 Note:
 A valid Sudoku board (partially filled) is not necessarily solvable. Only the filled cells need to be validated.
 Return 0 / 1 ( 0 for false, 1 for true ) for this problem
'''


'''
Solution Outline:
	1. Maintain lookup tables for the nine rows, columns and mini-3x3 grids
	2. The mini-3x3 grids can be indexed using (row/3*3+col/3) as index.
'''
class Solution:
	# @param A : tuple of strings
	# @return an integer
	def isValidSudoku(self, A):
		row_lookup = [set() for _ in xrange(9)]
		col_lookup = [set() for _ in xrange(9)]
		mini_grid_lookup = [set() for _ in xrange(9)]

		for row in xrange(9):
			for col in xrange(9):
				x = A[row][col]
				if x == '.':
					continue

				if x in row_lookup[row]:
					return 0
				if x in col_lookup[col]:
					return 0
				if x in mini_grid_lookup[row/3*3+col/3]:
					return 0

				row_lookup[row].add(x)
				col_lookup[col].add(x)
				mini_grid_lookup[row/3*3+col/3].add(x)

		return 1




if __name__ == '__main__':
	s = Solution()
	A = \
			[
				"..5.....6",
				"....14...",
				".........",
				".....92..",
				"5....2...",
				".......3.",
				"...54....",
				"3.....42.",
				"...27.6.."
			]
	assert s.isValidSudoku(A) == 1

	B = \
			[
				"..5.....6",
				"....14...",
				".........",
				".....92..",
				"5....2...",
				".......3.",
				"...54....",
				"3.....42.",
				"...27.6.4"
			]
	assert s.isValidSudoku(B) == 0



