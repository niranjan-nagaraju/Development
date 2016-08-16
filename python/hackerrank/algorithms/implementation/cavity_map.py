'''
https://www.hackerrank.com/challenges/cavity-map

You are given a square map of size n x n. Each cell of the map has a value denoting its depth. We will call a cell of the map a cavity if and only if this cell is not on the border of the map and each cell adjacent to it has strictly smaller depth. Two cells are adjacent if they have a common side (edge).

You need to find all the cavities on the map and depict them with the uppercase character X.

Input Format
The first line contains an integer, n, denoting the size of the map. Each of the following lines contains positive digits without spaces. Each digit (1-9) denotes the depth of the appropriate area.

Constraints
1 <= n <= 100

Output Format
Output n lines, denoting the resulting map. Each cavity should be replaced with character X.

Sample Input
4
1112
1912
1892
1234

Sample Output
1112
1X12
18X2
1234

'''

from __future__ import print_function
import sys

def is_cavity(grid, i, j):
	depth = grid[i][j]

	# compare adjacent cells 
	if ((depth > grid[i][j+1]) and (depth > grid[i][j-1]) and (depth > grid[i-1][j]) and (depth > grid[i+1][j])):
		return True

	return False


n = int(raw_input().strip())
grid = []
i = 0
for i in xrange(n):
	grid_t = map(int, raw_input().strip())
	grid.append(grid_t)


# print first row
for i in xrange(0, n):
	print(grid[0][i], end="")
print()

for i in xrange(1, n-1):
	# print border cells
	print(grid[i][0], end="")
	for j in xrange(1, n-1):
		print(grid[i][j], end="") if not is_cavity(grid, i, j) else print('X', end="")

	# print border cells to the right
	print(grid[i][n-1])

# print last row (except when n==1, in which case this will be a duplicate)
if (n == 1):
	exit(0)

for i in xrange(0, n):
	print(grid[n-1][i], end="")
print()
