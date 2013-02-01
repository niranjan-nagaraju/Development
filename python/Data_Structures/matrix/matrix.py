
class Matrix:
	def __init__(self, rows=None, cols=None, matrix=None):
		self.rows = rows
		self.cols = cols
		self.matrix = []

		# If the row is not specified, treat it as an empty matrix
		if not rows:
			self.rows = self.cols = 0
			return
	
		# When column is not specified, Create a square matrix with row x row dimensions
		if not cols: # Square matrix
			self.cols = rows

		# No contents specified for matrix OR the input matrix is not a list
		if (not matrix) or (not isinstance(matrix, list)):
			return
		
		if isinstance(matrix[0], list):	# We have a proper 2D list
			self.matrix = matrix
		else: # it's a flattened 1D list
			# TODO: Use list comprehension
			for i in range(0, rows):
				self.matrix.append([])	# open new row
				for j in range(0, cols):
					self.matrix[i].append(matrix[i*cols+j])


	# Prepare formatted string for print()
	def __str__(self):
		return str(self.rows) + "x" + str(self.cols) + \
				": " + str(self.matrix)


	# Return a deep copy of the current matrix
	def copy(self):
		return Matrix(self.rows, self.cols, self.matrx[:])

	
	# Check if two matrices are identical
	def __eq__(self, other):
		if (self.rows != other.rows) or (self.cols != other.cols):
			return False

		return self.matrix == other.matrix	


	# Verify Compatibility for addition/subtraction
	@staticmethod
	def add_compatible(m1, m2):
		# Matrix addition is not defined if the rows and columns don't match
		if (m1.rows != m2.rows or m1.cols != m2.cols):
			return False
		
		if not m1 or not m2:
			return False

		return True

	# Verify Compatibility for multiplication
	@staticmethod
	def mult_compatible(m1, m2):
		# Matrix multiplication is only defined if the columns of lhs matrix == rows of rhs matrix
		if (self.cols != other.rows):
			return False

		if not m1 or not m2:
			return False

		return True


	# Add 2 matrices
	def __add__ (self, other):
		# TODO: Use exceptions for corner conditions and cleaner code

		# Check compatibility before adding
		if not Matrix.add_compatible(self, other):
			return None

		# Store a copy of current matrix into the result matrix
		sum_matrix = self.copy()

		# Add the elements from the rhs matrix
		for i in range(0, self.rows):
			for j in range(0, self.cols):
				sum_matrix.matrix[i][j] += other.matrix[i][j]

		return sum_matrix


	def __sub__(self, other):
		# TODO: Use exceptions for corner conditions and cleaner code

		# Check compatibility before subtracting
		if not Matrix.add_compatible(self, other):
			return None

		# Store a copy of current matrix into the result matrix
		diff_matrix = self.copy()
		for i in range(0, self.rows):
			for j in range(0, self.cols):
				diff_matrix.matrix[i][j] -= other.matrix[i][j]

		return diff_matrix


	def __mul__(self, other):
		if not Matrix.mult_compatible(self, other):
			return None

		# A:mxp, B:pxn, A.B: mxn
		prod_matrix = Matrix(self.rows, other.cols)

		for i in range(0, prod_matrix.rows):
			prod_matrix.matrix.append([])	# prepare empty rows
			for j in range(0, prod_matrix.cols):
				prod_matrix.matrix[i].append(0)	# Create column j and set it to 0 -> matrix[i][j] = 0
				for k in range(0, self.cols):
					prod_matrix.matrix[i][j] += (self.matrix[i][k] * other.matrix[k][j])

		return prod_matrix

	def isIdentity(self):
		# All Identity matrices are square matrices
		if self.cols != self.rows:
			return False

		for i in range(0, self.rows):
			for j in range(0, self.rows):
				if (i != j):
					if (self.matrix[i][j] != 0):
						return False
				else:
					if (self.matrix[i][i] != 1):
						return False

		return True

	@staticmethod
	def getIdentityMatrix(order):
		iMatrix = Matrix(order)

		for i in range(0, order):	# Prepare the rows
			iMatrix.matrix.append([])
			for j in range(0, order):
				iMatrix.matrix[i].append(0)	# Initialize all columns of current row to zero
		
		for i in range(0, order):
			iMatrix.matrix[i][i] = 1

		return iMatrix

	# Transpose a matrix, rows <-> columns
	def transpose(self):
		tm = Matrix(self.cols, self.rows)

		for i in range(0, tm.rows):
			tm.matrix.append([])	# Prepare the rows
			for j in range(0, tm.cols):
				tm.matrix[i].append(self.matrix[j][i])

		return tm

