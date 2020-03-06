import numpy as np

class Solution:
	# @param n : integer
	# @return a list of list of integers
	def generateMatrix(self, n):
		spiral_matrix = [[0]*n for _ in xrange(n)]
		self.spiral_fill_array(spiral_matrix, 0, 1, n)
		return spiral_matrix


	def fill_right(self, matrix, r, c, num, n):
		for i in xrange(n):
			matrix[r][c+i] = num+i

	def fill_down(self, matrix, r, c, num, n):
		for i in xrange(n):
			matrix[r+i][c] = num+i

	def fill_left(self, matrix, r, c, num, n):
		for i in xrange(n):
			matrix[r][c-i] = num+i

	def fill_up(self, matrix, r, c, num, n):
		for i in xrange(n):
			matrix[r-i][c] = num+i


	def spiral_fill_array(self, spiral_matrix, level, num, n):
		if n <= 0:
			return

		self.fill_right(spiral_matrix, level+0, level+0, num, n)
		num += n

		self.fill_down(spiral_matrix, level+1, level+n-1, num, n-2)
		num += n-2

		self.fill_left(spiral_matrix, level+n-1, level+n-1, num, n)
		num += n

		self.fill_up(spiral_matrix, level+n-2, level+0, num, n-2)
		num += n-2

		# Go one level deeper
		return self.spiral_fill_array(spiral_matrix, level+1, num, n-2)



if __name__ == '__main__':
	s = Solution()
	#print np.matrix(s.generateMatrix(5))
	assert s.generateMatrix(1) == [[1]]
	assert s.generateMatrix(2) == [
			[1,2],
			[4,3]]
	assert s.generateMatrix(3) == [
			[1, 2, 3],
			[8, 9, 4],
			[7, 6, 5]]
	assert s.generateMatrix(4) == [
			[1,  2,  3,  4],
			[12, 13, 14, 5],
			[11, 16, 15, 6],
			[10, 9, 8, 7]]
	assert s.generateMatrix(5) == [
			[1,  2,  3,  4,  5],
			[16, 17, 18, 19, 6],
			[15, 24, 25, 20, 7],
			[14, 23, 22, 21, 8],
			[13, 12, 11, 10, 9]]
	assert s.generateMatrix(6) == [
			[1,  2,  3,  4,  5,  6],
			[20, 21, 22, 23, 24, 7],
			[19, 32, 33, 34, 25, 8],
			[18, 31, 36, 35, 26, 9],
			[17, 30, 29, 28, 27, 10],
			[16, 15, 14, 13, 12, 11]]

