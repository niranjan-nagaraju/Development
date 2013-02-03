'''
	Implement Matrix Operations for use with hill-cipher and other matrix based crypto-primitives
'''

import sys

class Matrix:
	def __init__(self, rows=None, cols=None, matrix=None):
		self.rows = rows
		self.cols = cols

		# If the row is not specified, Create an empty matrix
		if not rows:
			# Reset matrix, rows and columns
			self.rows, self.cols = 0,0
			self.matrix = []
			return
	
		# When column is not specified, Create a square matrix with row x row dimensions
		if not cols: # Square matrix
			self.cols = rows
		elif not isinstance (cols, int): # Second parameter was not an int, perhaps a list => square matrix
			self.cols = rows
			self.fromList(cols)
			return
			
		# Construct matrix elements from a 1d/2d list
		self.fromList(matrix)
	

	
	# Check if the matrix is empty
	def empty(self):
		return (not self.matrix)



	# Return a deep copy of the current matrix
	def copy(self):
		m = Matrix(self.rows, self.cols)
		m.matrix = self.matrix[:]
		return m



	# Return a matrix constructed from a list (1d or 2d)
	def fromList (self, matrix=None):
		self.matrix = []

		# None/[] passed to matrix
		if not matrix:
			return

		try:
			if isinstance(matrix[0], list):	# We have a proper 2D list
				self.matrix = matrix
			elif isinstance(matrix, list): # it's a flattened 1D list
				# Not enough elements in the specified matrix
				if len(matrix) != self.rows*self.cols:
					# Reset matrix, rows and columns
					self.rows, self.cols = 0,0
					return

				# Use list comprehension to 'fold' into a 2D list
				# [matrix[0:cols], matrix[cols:2*cols], ....]
				self.matrix = [ matrix[i:i+self.cols] for i in range(0, len(matrix), self.cols) ]
			else: # Tuple/another list like structure passed
				# Reset matrix, rows and columns
				self.rows, self.cols = 0,0
				return
		except: # AttributeError, TypeError - Just reset the matrix, rows and columns
			# print 'Unexpected error occured while creating matrix:', sys.exc_info()[0]
			self.rows, self.cols = 0,0



	# Prepare formatted string for print()
	def __str__(self):
		return str(self.rows) + "x" + str(self.cols) + \
				": " + str(self.matrix)

	
	# Check if two matrices are identical
	def __eq__(self, other):
		# If we are comparing against another matrix, compare rows, cols and elements in the matrix
		if isinstance(other, Matrix):
			if (self.rows != other.rows) or (self.cols != other.cols):
				return False
			return self.matrix == other.matrix	

		# If we are comparing against another list (assumed to be 2D), compare only the elements in the matrix
		if isinstance(other, list):
			return self.matrix == other


	# Verify Compatibility for addition/subtraction
	@staticmethod
	def add_compatible(m1, m2):
		# Matrix addition is not defined if the rows and columns don't match
		if (m1.rows != m2.rows or m1.cols != m2.cols):
			return False
		
		return True

	
	# Helper function to add or subtract 2 matrices
	def __add_or_sub__ (self, other, addsubfn):
		# TODO: Use exceptions for corner conditions and cleaner code

		if self.empty() or other.empty():
			return None

		# Check compatibility before adding/subtracting
		if not Matrix.add_compatible(self, other):
			return None

		# Add/subtract the elements from the rhs matrix
		result_matrix = Matrix(self.rows, self.cols)
		result_matrix.matrix = [ [addsubfn(self.matrix[i][j], other.matrix[i][j]) for j in range(self.cols)] for i in range(self.rows) ]

		return result_matrix



	# Add 2 matrices
	def __add__ (self, other):
		return self.__add_or_sub__ (other, int.__add__) # Pass integer addition function to add/sub



	# Subtract 2 matrices
	def __sub__(self, other):
		return self.__add_or_sub__ (other, int.__sub__) # Pass integer subtraction function to add/sub



	# Calculate dot-product of a matrix (Scalar multiplication)
	def dot_product(self, multiplier):
		return [ [self.matrix[i][j]*multiplier for j in range(self.cols)] for i in range(self.rows) ]



	# Verify Compatibility for vector product
	def vector_prod_compatible(self, other):
		if other.empty():
			return False

		# Matrix multiplication is only defined if the columns of lhs matrix == rows of rhs matrix
		return (self.cols == other.rows)


	
	# Calculate vector product of two matrices (Cross-product)
	def vector_product(self, other):
		if not self.vector_prod_compatible(other):
			return None

		# A:mxp, B:pxn, A.B: mxn
		# Create a zero-matrix of mxn dimensions
		prod_matrix = Matrix(self.rows, other.cols, [0]*(self.rows * other.cols))

		for i in range(0, prod_matrix.rows):
			for j in range(0, prod_matrix.cols):
				# set matrix[i][j] = 0 in product matrix
				# prod_matrix[i][j] = 0 -- not reqd
				for k in range(0, self.cols):
					prod_matrix.matrix[i][j] += (self.matrix[i][k] * other.matrix[k][j])

		return prod_matrix



	# Multiply 2 matrices
	# Dot product if rhs is an integer
	# Vector product, otherwise
	def __mul__(self, other):
		if self.empty():
			return None

		if isinstance(other, int):
			return self.dot_product(other)
		elif isinstance(other, Matrix):
			return self.vector_product(other)



	# Check if current matrix is an Identity Matrix
	def isIdentity(self):
		# NOTE: All Identity matrices are square matrices
		return self.matrix == Matrix.getIdentityMatrix(self.rows)



	# Create and return an Identity matrix with the specified dimension
	# NOTE: All Identity matrices are square matrices
	@staticmethod
	def getIdentityMatrix(order):
		id_matrix = [ [1 if i==j else 0 for j in range(order)] for i in range(order) ]
		return Matrix(rows=order, matrix=id_matrix)



	# Transpose a matrix, rows <-> columns
	def transpose(self):
		return Matrix(self.cols, self.rows,
				[[row[j] for row in self.matrix] for j in range(self.cols)])

	
	# Upper Triangular matrix
	def upper_triangle(self):
		# Triangular matrices are always square
		if self.cols != self.rows:
			return None

		return Matrix (self.rows, 
				[ [self.matrix[i][j] if i<=j else 0 for j in range(self.rows)] for i in range(self.rows)])


	# Lower Triangular matrix
	def lower_triangle(self):
		# Triangular matrices are always square
		if self.cols != self.rows:
			return None

		return Matrix (self.rows, 
				[ [self.matrix[i][j] if i>=j else 0 for j in range(self.rows)] for i in range(self.rows)])

