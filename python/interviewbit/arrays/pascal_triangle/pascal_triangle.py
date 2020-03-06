#encoding: utf-8
'''
https://www.interviewbit.com/problems/pascal-triangle/

Pascal Triangle

Given numRows, generate the first numRows of Pascal’s triangle.

Pascal’s triangle : To generate A[C] in row R, sum up A’[C] and A’[C-1] from previous row R - 1.

Example:

Given numRows = 5,

Return

[
     [1],
     [1,1],
     [1,2,1],
     [1,3,3,1],
     [1,4,6,4,1]
]
'''

class Solution:
	# @param numRows : integer
	# @return a list of list of integers
	def solve(self, numRows):
		triangle = [[1], [1,1]]

		if numRows < 3:
			return triangle[:numRows]

		for i in xrange(2, numRows):
			last_row = triangle[-1]
			row = [1]
			for j in xrange(len(last_row)-1):
				row.append(last_row[j] + last_row[j+1])
			row.append(1)

			triangle.append(row)

		return triangle



if __name__ == '__main__':
	s = Solution()
	assert s.solve(0) == []
	assert s.solve(1) == [[1]]
	assert s.solve(2) == [[1], [1, 1]]
	assert s.solve(3) == [[1], [1, 1], [1, 2, 1]]
	assert s.solve(4) == [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1]]
	assert s.solve(5) == [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1,4,6,4,1]]
	assert s.solve(6) == [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1,4,6,4,1], [1,5,10,10,5,1]]

