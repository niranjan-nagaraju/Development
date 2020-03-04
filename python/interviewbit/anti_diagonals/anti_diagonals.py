'''
https://www.interviewbit.com/problems/anti-diagonals/

Anti Diagonals

Give a N*N square matrix, return an array of its anti-diagonals. Look at the example for more details.

Example:
Input: 	
1 2 3
4 5 6
7 8 9

Return the following :
[ 
  [1],
  [2, 4],
  [3, 5, 7],
  [6, 8],
  [9]
]

Input : 
1 2
3 4

Return the following  : 
[
  [1],
  [2, 3],
  [4]
]
'''

'''
Solution Outline:
	Use BFS where the neighboring nodes are to the right, and down.
	Each sub-list contains a list of elements at the same level.
	NOTE: Levels for a N*N matrix would be
	    1, 2, 3, ..., N, N-1, N-2, ..., 2, 1
		Num levels =  N + N-1 = 2*N-1
		e.g., N=2, num_levels = 2+1 = 3
		N=3, num_levels = 3+2 = 5
'''

from collections import defaultdict
class Solution:
	def find_anti_diagonals(self, M):
		def add_to_queue(a, b, l):
			if a < 0 or a >= n:
				return
			if b < 0 or b >= n:
				return
			if not visited[(a,b)]:
				visited[(a,b)] = True
				bfs_q.append(((a,b),l))

		n = len(M) # square matrix, so r == c == n
		visited = defaultdict(lambda: False)
		bfs_q = [((0,0), 0)]
		anti_diags = [[] for _ in xrange(2*n-1)] # number of levels = 2n-1
		while bfs_q:
			(x,y),level = bfs_q.pop(0)
			anti_diags[level].append(M[x][y])
			add_to_queue(x, y+1, level+1) # right first
			add_to_queue(x+1, y, level+1) # down second

		return anti_diags


if __name__ == '__main__':
	s = Solution()
	m1 = [
			[1, 2, 3],
			[4, 5, 6],
			[7, 8, 9]]

	assert s.find_anti_diagonals(m1) == [
			  [1],
			  [2, 4],
			  [3, 5, 7],
			  [6, 8],
			  [9]]

	m2 = [
			[1, 2],
			[3, 4]]
	assert s.find_anti_diagonals(m2) == [
			[1],
			[2, 3],
			[4]]

	m3 = [
			[1, 2, 3, 4],
			[5, 6, 7, 8],
			[9, 8, 7, 6],
			[5, 4, 3, 2]]
	
	assert s.find_anti_diagonals(m3) == [
			[1],
			[2, 5],
			[3, 6, 9],
			[4, 7, 8, 5],
			[8, 7, 4],
			[6, 3],
			[2]]

