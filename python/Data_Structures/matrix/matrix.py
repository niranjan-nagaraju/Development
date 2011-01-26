#!/usr/bin/python

class Matrix:
	def __init__(self, rows=None, cols=None, matrix=None):
		self.rows = rows
		self.cols = cols
		self.matrix = matrix
		
		if cols ==  None:
			self.cols = rows

		if matrix == None:
			self.matrix = []


	# Initialize matrix from list
	def matrixFromList(self, rows, cols, matrix_list):
		self.rows = rows
		self.cols = cols
		self.matrix = []

		for i in range(0, rows):
			self.matrix.append([])	# open new row
			for j in range(0, cols):
				self.matrix[i].append(matrix_list[i*cols+j])

	# Prepare formatted string for print()
	def __str__(self):
		return str(self.rows) + "x" + str(self.cols) + \
			"\n" + str(self.matrix)


	def __add__ (self, other):
		if (self.rows != other.rows and self.cols != other.cols):
			return None

		if (self.matrix == None or other.matrix == None):
			return None

		sum_matrix = Matrix(self.rows, self.cols)
		for i in range(0, self.rows):
			sum_matrix.matrix.append([])
			for j in range(0, self.cols):
				sum_matrix.matrix[i].append(self.matrix[i][j] + other.matrix[i][j])

		return sum_matrix



	

m1 = Matrix()
m1.matrixFromList(2, 3, [1,2,3,4,5,6])
print m1

m2 = Matrix(2, 3, [[2,4,6], [8, 10, 12]])
print m2

m3 = m1 + m2
print m3
