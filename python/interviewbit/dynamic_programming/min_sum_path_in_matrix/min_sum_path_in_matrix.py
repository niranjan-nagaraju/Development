'''
https://www.interviewbit.com/problems/min-sum-path-in-matrix/

Min Sum Path in Matrix

Problem Description
Given a 2D integer array A of size M x N, you need to find a path from top left to bottom right which minimizes the sum of all numbers along its path.

NOTE: You can only move either down or right at any point in time.

Input Format
First and only argument is an 2D integer array A of size M x N.

Output Format
Return a single integer denoting the minimum sum of a path from cell (1, 1) to cell (M, N).

Example Input
Input 1:
 A = [  [1, 3, 2]
        [4, 3, 1]
        [5, 6, 1]
     ]

Example Output
Output 1:
 9

Explanation:
 The path is 1 -> 3 -> 2 -> 1 -> 1
 So ( 1 + 3 + 2 + 1 + 1) = 8
'''

'''
Solution Outline:
	1. Let's say the minimum cost to reach cell(M-2, N-1) (one cell above)  from (0,0) is x
		and the minimum cost to reach cell(M-1, N-2) (one cell left) from (0,0) is y
		Then, the minimum cost to reach cell(M-1, N-1) would be min(x, y) + A[M-1][N-1]
	2. Let f(i, j) be a function that returns the minimum cost to cell(i,j) from cell(0,0).
		Therefore, for any cell(i,j), the minimum cost to reach it would be 
		min(f(i-1,j), f(i, j-1)) + A[i][j]
	3. f(0,j) = A[0][j] -> As there is only one path to cell(0,j) (R-R-R...) [0<=j<N]
		f(i,0) = A[i][0] -> there is only one path to cell(i, 0) (D-D-D....) [0<=i<M]
	4. Use an T[][] MxN DP table to fill up minimum costs bottom-up.
		T[M-1][N-1] contains the minimum cost to cell(M-1, N-1)


Sample run:
	A:	[
			[1, 3, 2],
			[4, 3, 1],
			[5, 6, 1]
		]

	T: 
	| A | 0  | 1 | 2 |
	|---+----+---+---|
	| 0 | 1  | 4 | 6 |
	| 1 | 5  |   |   |
	| 2 | 10 |   |   |

	T[1][1]:
	| A | 0  | 1 | 2 |
	|---+----+---+---|
	| 0 | 1  | 4 | 6 |
	| 1 | 5  | 7 |   |
	| 2 | 10 |   |   |

	T[1][2]:
	| A | 0  | 1 | 2 |
	|---+----+---+---|
	| 0 | 1  | 4 | 6 |
	| 1 | 5  | 7 | 7 |
	| 2 | 10 |   |   |

	T[2][1]:
	| A | 0  | 1  | 2 |
	|---+----+----+---|
	| 0 | 1  | 4  | 6 |
	| 1 | 5  | 7  | 7 |
	| 2 | 10 | 13 |   |

	T[2][2]:
	| A | 0  | 1  | 2 |
	|---+----+----+---|
	| 0 | 1  | 4  | 6 |
	| 1 | 5  | 7  | 7 |
	| 2 | 10 | 13 | 8 |

	return 8
'''
class Solution:
	def find_min_sum_path_cost(self, A):
		if not A or not A[0]:
			return 0

		m = len(A)
		n = len(A[0])
		T = [[0 for _ in xrange(n)] for _ in xrange(m)]
		T[0][0] = A[0][0]

		# T[0][j] = sum(A[0][0], ... A[0][j])
		for j in xrange(1,n):
			T[0][j] = T[0][j-1]+A[0][j]

		# T[i][0] = sum(A[0][0], ... A[i][0])
		for i in xrange(1,m):
			T[i][0] = T[i-1][0]+A[i][0]

		
		# T[i][j] = min(T[i-1][j], T[i][j-1]) + A[i][j]
		for i in xrange(1, m):
			for j in xrange(1, n):
				T[i][j] = min(T[i-1][j], T[i][j-1]) + A[i][j]

		return T[-1][-1]


if __name__ == '__main__':
	s = Solution()
	assert s.find_min_sum_path_cost([
								[1, 3, 2],
								[4, 3, 1],
								[5, 6, 1]
							]) == 8

	assert s.find_min_sum_path_cost([
								[1, 2, 3],
								[4, 5, 6],
								[7, 8, 9]
							]) == 21
