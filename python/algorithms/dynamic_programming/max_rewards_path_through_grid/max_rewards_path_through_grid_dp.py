#encoding: utf-8
'''
Given an mxn grid where each cell has a reward associated with it, and a robot placed at the bottom-left cell,
Find the path to the top-right cell with the maximum reward.
The robot can only move UP or RIGHT.
'''

'''
Solution Outline:
	DP: O(m*n) time, O(m*n) memory

	Let's say the robot is one step away from the goal (top-right cell).
		Let Maximum reward from the left cell is x, and the maximum reward from the cell below is y
		Maximum reward for cell (0, n-1) is therefore maximum(x,y + reward at[0, n-1])

		If max_reward(i,j) is the maximum reward that can be obtained upon reaching cell(i,j)
		Then,
			max_reward(0, n-1) = max(max_reward(0, n-2), max_reward(1,n-1))
			max_reward(i, j) = max(max_reward(i, j-1), max_reward(i+1, j))  {0<=i<m, 0<=j<n}
			Base cases:
				max_reward(m-1, 0) = cell[m-1][0] {starting cell, bottom-left}
				max_reward(i, 0) = sum(cell[x][0], i<=x<m) {only direction to move is UP}
				max_reward(m-1, j) = sum(cell[m-1][x], 0<x<=j) {only direction to move is RIGHT}


	0. Initialize a DP table of mxn, retrace-path table of mxn
	1. DP[m-1][j] = sum(cell[m-1][x], 0<x<=j) {only direction to move is RIGHT}
		DP[i][0] = sum(cell[x][0], i<=x<m) {only direction to move is UP}
		Store previous paths in retrace-path table
	2. DP[i][j] = max(DP[i][j-1], DP[i+1][j]) + cell(i,j)
		Store previous paths in retrace-path based on which of left/down returned better rewards
	3. Return DP[0][n-1]

e.g.,
	4 8 3
	3 5 6
	1 7 2

max-reward: 1+7+5+8+3 = 24
'''
import numpy as np
class GridWithRewards(object):
	def __init__(self, grid):
		self.grid = grid
		self.m = len(grid)
		self.n = len(grid[0])
		self.DP = None
		self.retrace = None

	def find_max_rewards(self):
		if not self.DP:
			self.make_dp_table()
		return self.DP[0][-1]


	def retrace_max_rewards_path(self):
		if not self.DP:
			self.make_dp_table()

		cell = 0, self.n-1
		path = []
		while cell is not None:
			path.insert(0, cell)
			cell = self.retrace[cell[0]][cell[1]]

		return path


	def make_dp_table(self):
		self.DP = [[self.grid[i][j] for j in xrange(self.n)] for i in xrange(self.m)]
		self.retrace = [[None for _ in xrange(self.n)] for _ in xrange(self.m)]
		for i in xrange(self.m-2, -1, -1):
			# First column
			self.DP[i][0] += self.DP[i+1][0]
			self.retrace[i][0] = (i+1, 0) # only path is UP

		for j in xrange(1,self.n):
			# Last row
			self.DP[self.m-1][j] += self.DP[self.m-1][j-1]
			self.retrace[self.m-1][j] = (self.m-1, j-1) # only path is RIGHT

		
		for i in xrange(self.m-2, -1, -1):
			for j in xrange(1, self.n):
				if self.DP[i][j-1] > self.DP[i+1][j]:
					self.DP[i][j] += self.DP[i][j-1]
					self.retrace[i][j] = (i,j-1)
				else:
					self.DP[i][j] += self.DP[i+1][j]
					self.retrace[i][j] = (i+1,j)
	


if __name__ == '__main__':
	g = GridWithRewards([[4,8,3], [3,5,6], [1,7,2]])
	assert g.find_max_rewards() == 24
	assert g.retrace_max_rewards_path() == [(2,0), (2,1), (1,1), (0,1), (0,2)]

	g = GridWithRewards(\
			[
				[4, 2, 1, 5],
				[4, 8, 3, 3],
				[3, 5, 6, 2],
				[1, 7, 2, 1]
			])
	assert g.find_max_rewards() == 32
	assert g.retrace_max_rewards_path() == [(3, 0), (3, 1), (2, 1), (1, 1), (1, 2), (1, 3), (0, 3)]

