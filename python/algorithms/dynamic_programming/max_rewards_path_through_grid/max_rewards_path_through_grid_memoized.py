#encoding: utf-8
'''
Given an mxn grid where each cell has a reward associated with it, and a robot placed at the bottom-left cell,
Find the path to the top-right cell with the maximum reward.
The robot can only move UP or RIGHT.
'''

'''
Solution Outline:
	Brute-force (exhaustive-search, O(2ᵐⁿ)), memoized

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

	def find_max_rewards(self):
		def max_rewards(i, j):
			try:
				x = memoized[(i,j)]
				return x
			except KeyError:
				pass

			if (j == 0):
				# First column
				memoized[(i,j)] = sum([self.grid[x][0] for x in xrange(self.m-1, i-1, -1)]), [(x,0) for x in xrange(self.m-1, i-1, -1)]
			elif (i == self.m-1):
				# Last row
				memoized[(i,j)] = sum([self.grid[self.m-1][x] for x in xrange(0, j+1)]), [(self.m-1, x) for x in xrange(0, j+1)]
			else:
				left_rewards, lpath = max_rewards(i, j-1)
				down_rewards, dpath = max_rewards(i+1, j)

				memoized[(i,j)] = (self.grid[i][j] + max(left_rewards, down_rewards)), \
						(lpath + [(i,j)]) if (left_rewards > down_rewards) else (dpath + [(i,j)])
			return memoized[(i,j)]

		memoized = {}
		rewards, path = max_rewards(0, self.n-1)
		return rewards, path


if __name__ == '__main__':
	assert GridWithRewards([[4,8,3], [3,5,6], [1,7,2]]).find_max_rewards()[0] == 24
	assert GridWithRewards([[4,8,3], [3,5,6], [1,7,2]]).find_max_rewards()[1] == [(2,0), (2,1), (1,1), (0,1), (0,2)]

	g = GridWithRewards(\
			[
				[4, 2, 1, 5],
				[4, 8, 3, 3],
				[3, 5, 6, 2],
				[1, 7, 2, 1]
			])

	max_rewards, max_rewards_path = g.find_max_rewards()
	assert max_rewards == 32
	assert max_rewards_path == [(3, 0), (3, 1), (2, 1), (1, 1), (1, 2), (1, 3), (0, 3)]

