'''
https://www.interviewbit.com/problems/rotate-matrix/

Rotate Matrix
You are given an n x n 2D matrix representing an image.
Rotate the image by 90 degrees (clockwise).
You need to do this in place.
Note that if you end up using an additional array, you will only receive partial score.

Example:

If the array is

[
    [1, 2],
    [3, 4]
]
Then the rotated array becomes:

[
    [3, 1],
    [4, 2]
]
'''

'''
Solution Outline:
	2x2 matrix
	==========
	1 2   3 1
	3 4   4 2

	reverse each row
	2 1
	4 3

	reverse primary diagonal [2,3]
	3 1
	4 2

	3x3 matrix
	==========
	1 2 3    7 4 1
	4 5 6    8 5 2
	7 8 9    9 6 3

	reverse each row
	3 2 1
	6 5 4
	9 8 7

	reverse primary diagonal [3,5,7],    reverse [6,8]    reverse [2,4]
	7 2 1                                7 2 1            7 4 1
	6 5 4                                8 5 4            8 5 2
	9 8 3                                9 6 3            9 6 3

	4x4 matrix
	==========
	a b c d   m i e a
	e f g h   n j f b
	i j k l   o k g c
	m n o p   p l h d

	reverse each row   reverse [d,g,j,m]  reverse [h,k,n]  reverse [c,f,i]  reverse [l,o], [b,e]
	d c b a            m c b a            m c b a          m i b a          m i e a
	h g f e            h j f e            n j f e          n j f e          n j f b
	l k j i            l k g i            l k g i          l k g c          o k g c
	p o n m            p o n d            p o h d          p o h d          p l h d
	
	3x4 matrix
	==========
	a b c d     i e a  
	e f g h     j f b  
	i j k l     k g c
	            l h d



	Diagonals for a 4x4 matrix
	(0,0), (1,1), (2,2), (3,3)
	(0,1), (1,2), (2,3)
    (0,2), (1,3)
	(0,3)

	(1,0), (2,1), (3,2)
	(2,0), (3,1)
	(3,0)
'''
import numpy as np
class Solution:
	# @param matrix : list of list of integers
	# @return the same list modified
	def rotate_matrix(self, matrix):
		n = len(matrix)

		# reverse the row
		for i in xrange(n):
			self.reverse_row(matrix, i)

		# reverse primary diagonal and the diagonals above it
		for i in xrange(n):
			self.reverse_diagonal(matrix, 0, i, n-i)

		# reverse the diagonals below the primary diagonal
		for i in xrange(1, n):
			self.reverse_diagonal(matrix, i, 0, n-i)

		#print np.array(matrix)
		return matrix


	# Reverse elements in a row
	def reverse_row(self, matrix, row):
		i, j = 0, len(matrix)-1
		while i < j:
			matrix[row][i], matrix[row][j] = matrix[row][j], matrix[row][i]
			i += 1
			j -= 1


	# reverse elements along a diagonal starting at (i,j)
	def reverse_diagonal(self, matrix, i, j, diag_len):
		# start at (i.j) and move diagonally until (i+p, j+p) {i+p<n, j+p<n}
		# reverse the elements along the diagonal
		start = (i, j)
		end = (i+diag_len-1, j+diag_len-1)
		while start[0] < end[0]:
			matrix[start[0]][start[1]], matrix[end[0]][end[1]] = matrix[end[0]][end[1]], matrix[start[0]][start[1]]
			start = (start[0]+1, start[1]+1)
			end = (end[0]-1, end[1]-1)

    
if __name__ == '__main__':
	s = Solution()
	assert s.rotate_matrix([[1]]) == [[1]]
	assert s.rotate_matrix([
		[1,2],
		[3,4]]) == [
					[3,1],
					[4,2]
					]

	assert s.rotate_matrix([
		[1,2,3],
		[4,5,6],
		[7,8,9]
		]) == [
				[7, 4, 1],
				[8, 5, 2],
				[9, 6, 3]
			  ]

	assert s.rotate_matrix([
		['a', 'b', 'c', 'd'],
		['e', 'f', 'g', 'h'],
		['i', 'j', 'k', 'l'],
		['m', 'n', 'o', 'p']
		]) == [
				['m', 'i', 'e', 'a'],
				['n', 'j', 'f', 'b'],
				['o', 'k', 'g', 'c'],
				['p', 'l', 'h', 'd']
			  ]
