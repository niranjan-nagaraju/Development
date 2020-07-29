#encoding: utf-8
'''
https://www.interviewbit.com/problems/sudoku/

Sudoku

Write a program to solve a Sudoku puzzle by filling the empty cells.
Empty cells are indicated by the character '.'
You may assume that there will be only one unique solution.

+===+===+===+===+===+===+===+===+===+
❚ 5 | 3 |   ❚   | 7 |   ❚   |   |   ❚
+---+---+---+---+---+---+---+---+---+
❚ 6 |   |   ❚ 1 | 9 | 5 ❚   |   |   ❚
+---+---+---+---+---+---+---+---+---+
❚   | 9 | 8 ❚   |   |   ❚   | 6 |   ❚ 
+===+===+===+===+===+===+===+===+===+
❚ 8 |   |   ❚   | 6 |   ❚   |   | 3 ❚
+---+---+---+---+---+---+---+---+---+
❚ 4 |   |   ❚ 8 |   | 3 ❚   |   | 1 ❚
+---+---+---+---+---+---+---+---+---+
❚ 7 |   |   ❚   | 2 |   ❚   |   | 6 ❚
+===+===+===+===+===+===+===+===+===+
❚   | 6 |   ❚   |   |   ❚ 2 | 8 |   ❚
+---+---+---+---+---+---+---+---+---+
❚   |   |   ❚ 4 | 1 | 9 ❚   |   | 5 ❚
+---+---+---+---+---+---+---+---+---+
❚   |   |   ❚   | 8 |   ❚   | 7 | 9 ❚
+===+===+===+===+===+===+===+===+===+

A sudoku puzzle,

+===+===+===+===+===+===+===+===+===+
❚ 5 | 3 | 𝟜 ❚ 𝟞 | 7 | 𝟠 ❚ 𝟡 | 𝟙 | 𝟚 ❚
+---+---+---+---+---+---+---+---+---+
❚ 6 | 𝟟 | 𝟚 ❚ 1 | 9 | 5 ❚ 𝟛 | 𝟜 | 𝟠 ❚
+---+---+---+---+---+---+---+---+---+
❚ 𝟙 | 9 | 8 ❚ 𝟛 | 𝟜 | 𝟚 ❚ 𝟝 | 6 | 𝟟 ❚ 
+===+===+===+===+===+===+===+===+===+
❚ 8 | 𝟝 | 𝟡 ❚ 𝟟 | 6 | 𝟙 ❚ 𝟜 | 𝟚 | 3 ❚
+---+---+---+---+---+---+---+---+---+
❚ 4 | 𝟚 | 𝟞 ❚ 8 | 𝟝 | 3 ❚ 𝟟 | 𝟡 | 1 ❚
+---+---+---+---+---+---+---+---+---+
❚ 7 | 𝟙 | 𝟛 ❚ 𝟡 | 2 | 𝟜 ❚ 𝟠 | 𝟝 | 6 ❚
+===+===+===+===+===+===+===+===+===+
❚ 𝟡 | 6 | 𝟙 ❚ 𝟝 | 𝟛 | 𝟟 ❚ 2 | 8 | 𝟜 ❚
+---+---+---+---+---+---+---+---+---+
❚ 𝟚 | 𝟠 | 𝟟 ❚ 4 | 1 | 9 ❚ 𝟞 | 𝟛 | 5 ❚
+---+---+---+---+---+---+---+---+---+
❚ 𝟛 | 𝟜 | 𝟝 ❚ 𝟚 | 8 | 𝟞 ❚ 𝟙 | 7 | 9 ❚
+===+===+===+===+===+===+===+===+===+

and its solution numbers marked in red.

Example :
For the above given diagrams, the corresponding input to your program will be
	[[53..7....], [6..195...], [.98....6.], [8...6...3], [4..8.3..1], [7...2...6], [.6....28.], [...419..5], [....8..79]]
and we would expect your program to modify the above array of array of characters to
	[[534678912], [672195348], [198342567], [859761423], [426853791], [713924856], [961537284], [287419635], [345286179]]
'''

'''
Solution Outline:
	1. Scan for empty slots row-wise, left-right and fill one at a time with numbers
		that are allowed.
	2. Any time, any of 1-9 cannot be placed at a particular cell, backtrack to the previous position that was filled,
		and try to place +1 in its place, and repeat 2. until we are done with all the empty cells.
	3. As a slight optimization, each recursive call need not scan for empty slots from the top-left cell.
		Since we fill cells top-bottom, left-right, we can start from the last empty cell that was filled. 
	4. As an additional optimization, Use lookup tables for rows, columns and the mini 3x3 grids to quickly find if they contain
		a number 'n'.
'''

import numpy as np
toChr = lambda x: chr(x+ord('0'))
class Solution:

	def solveSudoku(self, grid):
		# check if it's safe to put in
		# 'n' in the grid at (x,y)
		def is_safe(x, y, n):
			# check row x
			if n in row_set[x]:
				return False

			# check column y
			if n in col_set[y]:
				return False

			# check the 3x3 grid
			if n in mini_grid_set[x/3*3+y/3]:
				return False

			return True


		# recursive helper to solve the board
		def solveHelper_r(startrow=0, startcol=0):
			try:
				# Start with scanning from (startrow, startcol)
				i = startrow
				for j in xrange(startcol, 9):
					if grid[i][j] == '.':
						raise StopIteration

				for i in xrange(startrow+1, 9):
					for j in xrange(0, 9):
						if grid[i][j] == '.':
							raise StopIteration
			except StopIteration:
				# found an empty cell
				for x in xrange(1, 10):
					if is_safe(i, j, toChr(x)):
						grid[i][j] = toChr(x)
						row_set[i].add(toChr(x))
						col_set[j].add(toChr(x))
						mini_grid_set[i/3*3+j/3].add(toChr(x))
						# We are filling cells top-bottom
						# skip checking all previous rows for empty cells
						# we would have filled them at this point
						solveHelper_r(i, j+1)
						grid[i][j] = '.' # backtrack
						row_set[i].remove(toChr(x))
						col_set[j].remove(toChr(x))
						mini_grid_set[i/3*3+j/3].remove(toChr(x))
				# Either we couldn't fit in any number in the current cell (i,j)
				# or one of the lower levels couldn't
				# return back to upper levels to retry with the next number
				#print np.matrix(grid)
				return
			else:
				# Couldn't find any empty cells
				# We have a complete board
				print np.matrix(grid)
				for row in xrange(9):
					orig_grid[row] = ''.join(grid[row])



		# Build row/col/mini-3x3 grid lookups	
		orig_grid = grid
		grid = map(list, [orig_grid[i] for i in xrange(9)])
		row_set = [set() for _ in xrange(9)]
		col_set = [set() for _ in xrange(9)]
		mini_grid_set = [set() for _ in xrange(9)]

		for row in xrange(9):
			for col in xrange(9):
				row_set[row].add(grid[row][col])
				col_set[col].add(grid[row][col])
				# The 3x3 grids are
				# (0,0)->(2,2), (0,3)->(2,5), (0,6)->(2,8)
				# (3,0)->(5,2), (3,3)->(5,5), (3,6)->(5,8)
				# (6,0)->(8,2), (6,3)->(8,5), (6,6)->(8,8)
				# cell(4,5) is grid (3,3)->(5,5) -> grid 4
				#  4/3*3 + 5/3 = 3+1 = 4
				# cell(7,6) is grid(6,6)->(8,8) -> grid 8
				#  7/3*3+6/3 = 6+2 == 8
				mini_grid_set[row/3*3+col/3].add(grid[row][col])

		solveHelper_r()
		return orig_grid


if __name__ == '__main__':
	s = Solution()
	grid = \
			[
				"53..7....",
				"6..195...",
				".98....6.",
				"8...6...3",
				"4..8.3..1",
				"7...2...6",
				".6....28.",
				"...419..5",
				"....8..79"
			]

	assert s.solveSudoku(grid) ==\
			[
				'534678912',
				'672195348',
				'198342567',
				'859761423',
				'426853791',
				'713924856',
				'961537284',
				'287419635',
				'345286179'
			]
	assert grid == \
			[
				'534678912',
				'672195348',
				'198342567',
				'859761423',
				'426853791',
				'713924856',
				'961537284',
				'287419635',
				'345286179'
			]

