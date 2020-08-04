'''
https://leetcode.com/problems/unique-paths/

62. Unique Paths

A robot is located at the top-left corner of a m x n grid (marked 'Start' in the diagram below).

The robot can only move either down or right at any point in time. The robot is trying to reach the bottom-right corner of the grid (marked 'Finish' in the diagram below).

How many possible unique paths are there?

|---+---+---+---+---+---+---|
| S |   |   |   |   |   |   |
|---+---+---+---+---+---+---|
|   |   |   |   |   |   |   |
|---+---+---+---+---+---+---|
|   |   |   |   |   |   | E |
|---+---+---+---+---+---+---|

Above is a 7 x 3 grid. How many possible unique paths are there?

Example 1:
Input: m = 3, n = 2
Output: 3
Explanation:
From the top-left corner, there are a total of 3 ways to reach the bottom-right corner:
1. Right -> Right -> Down
2. Right -> Down -> Right
3. Down -> Right -> Right

Example 2:
Input: m = 7, n = 3
Output: 28
 

Constraints:
1 <= m, n <= 100
It's guaranteed that the answer will be less than or equal to 2 * 10 ^ 9.
'''


'''
Solution Outline: O(n) DP using a single list of size n
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
|  1 |  1 |  1 |  1 |  1 |  1 |  1 |
|----+----+----+----+----+----+----|
|  1 |  2 |  3 |  4 |  5 |  6 |  7 |
|----+----+----+----+----+----+----|
|  1 |  3 |  6 | 10 | 15 | 21 | 28 |
|----+----+----+----+----+----+----|

'''
class Solution(object):
	def uniquePaths(self, m, n):
		"""
		:type m: int
		:type n: int
		:rtype: int
		"""
		DP = [1 for _ in xrange(n)]
		for i in xrange(1, m):
			for j in xrange(1, n):
				DP[j] += DP[j-1]

		return DP[-1]


if __name__ == '__main__':
	s = Solution()
	assert s.uniquePaths(2, 2) == 2
	assert s.uniquePaths(3, 7) == 28
	assert s.uniquePaths(3, 3) == 6

