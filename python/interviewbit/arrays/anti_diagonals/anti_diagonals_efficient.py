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
	Traverse the matrix row by row, adding items to their respective levels.
	M[i][j] will be added to AD[i+j]
	NOTE: Levels for a N*N matrix would be
	    1, 2, 3, ..., N, N-1, N-2, ..., 2, 1
		Num levels =  N + N-1 = 2*N-1
		e.g., N=2, num_levels = 2+1 = 3
		N=3, num_levels = 3+2 = 5
'''

class Solution:
	def find_anti_diagonals(self, M):
		n = len(M) # square matrix, so r == c == n
		anti_diags = [[] for _ in xrange(2*n-1)] # number of levels = 2n-1
		for i in xrange(n):
			for j in xrange(n):
				anti_diags[i+j].append(M[i][j])

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

