#encoding: utf-8
'''
https://www.interviewbit.com/problems/grid-unique-paths/

Grid Unique Paths

The robot can only move either down or right at any point in time. The robot is trying to reach the bottom-right corner of the grid (marked ‘Finish’ in the diagram below).

|---+---+---+---+---+---+---|
| S |   |   |   |   |   |   |
|---+---+---+---+---+---+---|
|   |   |   |   |   |   |   |
|---+---+---+---+---+---+---|
|   |   |   |   |   |   | E |
|---+---+---+---+---+---+---|

How many possible unique paths are there?

Note: A and B will be such that the resulting answer fits in a 32 bit signed integer.

Example :
	Input : A = 2, B = 2
	Output : 2
	2 possible routes : (0, 0) -> (0, 1) -> (1, 1) 
	              OR  : (0, 0) -> (1, 0) -> (1, 1)
'''

'''
Solution Outline: (DP)
	In an AxB grid, S=(0,0), E=(A-1, B-1)
	Let f(x,y) be the number of ways to reach cell (x,y) from S
	f(0,1) = Number of ways to reach (0,1) from S == 1 (R)
	f(1,0) = Number of ways to reach (1,0) from S == 1 (D)

	f(x,0) = Number of ways to reach (x,0) from S == 1 (DD...D)
	f(0,y) = Number of ways to reach (0,y) from S == 1 (RR...R)

	For any other cell (i,j), Paths to enter it are from its top and left-side cells.
	  Therefore the number of ways to reach cell (i,j) == f(i,j) = f(i-1, j) + f(i, j-1)

	Fill the table bottom-up, i: 0 to A, j: 0 to B, return f(A-1, B-1)

Sample run for the grid above:
|----+----+----+----+----+----+----|
|  0 |  1 |  1 |  1 |  1 |  1 |  1 |
|----+----+----+----+----+----+----|
|  1 |  2 |  3 |  4 |  5 |  6 |  7 |
|----+----+----+----+----+----+----|
|  1 |  3 |  6 | 10 | 15 | 21 | 28 |
|----+----+----+----+----+----+----|

'''

class Solution:
	def count_unique_paths(self, A, B):
		DP = [[0 for j in xrange(B)] for i in xrange(A)]
		for i in xrange(A):
			DP[i][0] = 1

		for j in xrange(B):
			DP[0][j] = 1

		for i in xrange(1, A):
			for j in xrange(1, B):
				DP[i][j] = DP[i-1][j] +  DP[i][j-1]

		return DP[-1][-1]


if __name__ == '__main__':
	s = Solution()
	assert s.count_unique_paths(2, 2) == 2
	assert s.count_unique_paths(3, 7) == 28
	assert s.count_unique_paths(3, 3) == 6

