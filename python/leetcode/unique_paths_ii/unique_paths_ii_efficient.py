'''
https://leetcode.com/problems/unique-paths-ii/

63. Unique Paths II

A robot is located at the top-left corner of a m x n grid (marked 'Start' in the diagram below).

The robot can only move either down or right at any point in time. The robot is trying to reach the bottom-right corner of the grid (marked 'Finish' in the diagram below).

Now consider if some obstacles are added to the grids. How many unique paths would there be?
An obstacle and empty space is marked as 1 and 0 respectively in the grid.

Note: m and n will be at most 100.

Example 1:
Input:
[
  [0,0,0],
  [0,1,0],
  [0,0,0]
]
Output: 2
Explanation:
There is one obstacle in the middle of the 3x3 grid above.
There are two ways to reach the bottom-right corner:
1. Right -> Right -> Down -> Down
2. Down -> Down -> Right -> Right
'''


'''
Solution Outline: Efficient DP O(n) memory
    0. Allowed directions are R, D
    1. Consider moving to cell x,y from 0,0
        If there were no obstacles, it would be (num_paths_to(x-1,y) + num_paths_to(x,y-1))
        with num_paths_to(x,0) == 1, (only direction allowed is down)
        and num_paths_to(0,y) == 1 (only direction allowed is right) {for any 0<=x<m,0<=y<n}
    2. With obstacles,
        if x,0 is an obstacle,
            then the column looks like (x=2 in the example)
            [[0
             [0
             [1
             [0
             [0 0 . . .
        num_paths_to(0,0) = 1
        num_paths_to(1,0) = 1
        num_paths_to(2,0) = 0 (blockade)
        num_paths_to(3,0) = 0 (can' get past blockade moving only D)
        num_paths_to(4,0) = 0

        Similarly, if (0,y) is an obstacle,
            then the first row looks like (y=1 in the example)
            [[0 1 0 0 0 0]
        num_paths_to(0,0) = 1
        num_paths_to(0,1) = 0 (blockade)
        num_paths_to(0,y) = 0 (for all y > 1) (can't get past blockade moving only R)

        For any random(x,y),
          if x,y is an obstacle, then num_paths_to(x,y) = 0
          otherwise,
          num_paths_to(x,y) = sum(num_paths_to(x-1,y), num_paths_to(x,y-1))

Sample run 1:
A= [
      [0,0,0],
      [0,1,0],
      [0,0,0]
    ]

DP: [
      [0,0,0],
      [0,0,0],
      [0,0,0]
    ]

Fill DP row 0,
DP: [
      [1,1,1],
      [0,0,0],
      [0,0,0]
    ]
Fill DP col 0,
DP: [
      [1,1,1],
      [1,0,0],
      [1,0,0]
    ]

(x,y): (1,1) is a blockade
DP: [
      [1,1,1],
      [1,0,0],
      [1,0,0]
    ]

(x,y): (1,2) == sum(left, up) == sum(DP[1,1], DP[0,2]) == 1
DP: [
      [1,1,1],
      [1,0,1],
      [1,0,0]
    ]

(x,y): (2,1) == sum(left,up) == sum(DP[2,0], DP[1,1]) == 1
DP: [
      [1,1,1],
      [1,0,1],
      [1,1,0]
    ]

(x,y): (2,2) == sum(left,up) == sum(DP[2,1], DP[1,2]) == 2
DP: [
      [1,1,1],
      [1,0,1],
      [1,1,2]
    ]
'''
class Solution(object):
	def uniquePathsWithObstacles(self, obstacleGrid):
		"""
		:type obstacleGrid: List[List[int]]
		:rtype: int
		"""

		if not obstacleGrid:
			return 0

		m = len(obstacleGrid)
		n = len(obstacleGrid[0])

		# End cell is blocked
		if obstacleGrid[-1][-1] == 1:
			return 0

		DP = [[0 for _ in xrange(n)] for _ in xrange(m)]

		# first row
		for j in xrange(n):
			if obstacleGrid[0][j] == 1:
				break
			DP[0][j] = 1

		for i in xrange(1, m):
			if obstacleGrid[i][0] == 1 or DP[(i-1)&1][0] == 0:
                # If the current row start is a blockade
                # or one of the previous rows had a blockade
                # in column 0, mark this row's column 0 as unreachable
				DP[i&1][0] = 0
			else:
				DP[i&1][0] = 1

			for j in xrange(1, n):
				if obstacleGrid[i][j] == 0:
					DP[i&1][j] = DP[(i-1)&1][j] + DP[i&1][j-1]
				else: # cell[i][j] is an obstacle, DP[i][j] is 0
					DP[i&1][j] = 0

		return DP[(m-1)&1][-1]


if __name__ == '__main__':
	s = Solution()
	assert s.uniquePathsWithObstacles(\
		[
		  [0,0,0],
		  [0,1,0],
		  [0,0,0]
		]) == 2

	assert s.uniquePathsWithObstacles(\
		[
		  [0,0,0],
		  [0,1,0],
		  [0,0,1]
		]) == 0

	assert s.uniquePathsWithObstacles(\
		[
		  [0,0,1,0],
		  [0,1,0,0],
		  [0,0,0,0],
		  [1,0,0,0]
		]) == 3

	assert s.uniquePathsWithObstacles(\
		[
		  [0,0,1,0],
		  [0,1,0,0],
		  [0,0,0,0],
		  [0,0,0,0],
		  [1,0,0,0]
		]) == 9


