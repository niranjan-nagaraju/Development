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

import numpy as np
toChr = lambda x: chr(x+ord('0'))
class Solution:
	# check if it's safe to put in
	# 'n' in the grid at (x,y)
	def is_safe(self, grid, x, y, n):
		# check row x
		for j in xrange(9):
			if grid[x][j] == n:
				return False

		# check column y
		for i in xrange(9):
			if grid[i][y] == n:
				return False


		# check the 3x3 grid
		# The 3x3 grids are
		# (0,0)->(2,2), (0,3)->(2,5), (0,6)->(2,8)
		# (3,0)->(5,2), (3,3)->(5,5), (3,6)->(5,8)
		# (6,0)->(8,2), (6,3)->(8,5), (6,6)->(8,8)
		# cell(4,5) is grid (3,3)->(5,5)
		#  (4/3),(5/3) == 1,1 -> *3 = 3,3 (start)
		# cell(7,6) is grid(6,6)->(8,8)
		# (7/3),(6/3) == 2,2 -> *3 == 6,6 (start)
		for i in xrange(3):
			for j in xrange(3):
				if grid[(x/3)*3+i][(y/3)*3+j] == n:
					return False

		return True


	# recursive helper to solve the board
	def solveHelper(self, grid, orig_grid, startrow=0, startcol=0):
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
				if self.is_safe(grid, i, j, toChr(x)):
					grid[i][j] = toChr(x)
					# We are filling cells top-bottom
					# skip checking all previous rows for empty cells
					# we would have filled them at this point
					self.solveHelper(grid, orig_grid, i, j+1)
					grid[i][j] = '.' # backtrack
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



	def solveSudoku(self, grid):
		gridList = map(list, [grid[i] for i in xrange(9)])
		self.solveHelper(gridList, grid)
		return grid


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

	#print np.matrix(gridList)

	assert s.is_safe(grid, 0,2,'6') == False
	assert s.is_safe(grid, 0,2,'1') == True
	assert s.is_safe(grid, 6,8,'1') == False
	assert s.is_safe(grid, 6,8,'1') == False
	assert s.is_safe(grid, 6,8,'3') == False

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
