'''
https://www.interviewbit.com/problems/unique-paths-in-a-grid/

Unique Paths in a Grid
Given a grid of size m * n, lets assume you are starting at (1,1) and your goal is to reach (m,n). At any instance, if you are on (x,y), you can either go to (x, y + 1) or (x + 1, y).

Now consider if some obstacles are added to the grids. How many unique paths would there be?
An obstacle and empty space is marked as 1 and 0 respectively in the grid.

Example :
	There is one obstacle in the middle of a 3x3 grid as illustrated below.
[
  [0,0,0],
  [0,1,0],
  [0,0,0]
]
The total number of unique paths is 2.

Note: m and n will be at most 100. 
'''


'''
Solution Outline: 
    (Efficient DP, same approach as solution #2 but uses a single row DP table of size n)
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
class Solution:
	def count_unique_paths_in_a_grid(self, A):
		if not A:
			return 0

		m = len(A)
		n = len(A[0])

		# End cell is blocked
		if A[-1][-1] == 1:
			return 0

		DP = [0 for _ in xrange(n)]

		# first row
		for j in xrange(n):
			if A[0][j] == 1:
				break
			DP[j] = 1

		for i in xrange(1, m):
			if A[i][0] == 1 or DP[0] == 0:
                # If the current row start is a blockade
                # or one of the previous rows had a blockade
                # in column 0, mark this row's column 0 as unreachable
				DP[0] = 0
			else:
				DP[0] = 1

			for j in xrange(1,n):
				if A[i][j] == 0:
					DP[j] = DP[j] + DP[j-1]
				else: # A[i][j] is an obstacle, DP[i][j] is 0
					DP[j] = 0

		return DP[-1]


if __name__ == '__main__':
	s = Solution()
	assert s.count_unique_paths_in_a_grid(\
		[
		  [0,0,0],
		  [0,1,0],
		  [0,0,0]
		]) == 2

	assert s.count_unique_paths_in_a_grid(\
		[
		  [0,0,0],
		  [0,1,0],
		  [0,0,1]
		]) == 0

	assert s.count_unique_paths_in_a_grid(\
		[
		  [0,0,1,0],
		  [0,1,0,0],
		  [0,0,0,0],
		  [1,0,0,0]
		]) == 3

	assert s.count_unique_paths_in_a_grid(\
		[
		  [0,0,1,0],
		  [0,1,0,0],
		  [0,0,0,0],
		  [0,0,0,0],
		  [1,0,0,0]
		]) == 9

