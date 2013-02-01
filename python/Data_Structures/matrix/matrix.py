
class Matrix:
	def __init__(self, rows=None, cols=None, matrix=None):
		self.rows = rows
		self.cols = cols
		self.matrix = []

		# If the row is not specified, treat it as an empty matrix
		if rows == None:
			self.rows = self.cols = 0
			return
	
		# When column is not specified, Create a square matrix with row x row dimensions
		if cols ==  None: # Square matrix
			self.cols = rows

		# No contents specified for matrix OR the input matrix is not a list
		if (matrix == None) or (not isinstance(matrix, list)):
			return
		
		if isinstance(matrix[0], list):	# We have a proper 2D list
			self.matrix = matrix
		else: # it's a flattened 1D list
			for i in range(0, rows):
				self.matrix.append([])	# open new row
				for j in range(0, cols):
					self.matrix[i].append(matrix[i*cols+j])


	# Prepare formatted string for print()
	def __str__(self):
		return str(self.rows) + "x" + str(self.cols) + \
				": " + str(self.matrix)


	def __add__ (self, other):
        #
        # TODO: Implementation Change 
        #   sum_matrix = copy(m1)
        #   sum_matrix += m2

        # Matrix addition is not defined if the rows and columns don't match
		if (self.rows != other.rows or self.cols != other.cols):
			return None

		if (self.matrix == None or other.matrix == None):
			return None

        # Create an empty matrix to hold the sum
		sum_matrix = Matrix(self.rows, self.cols)

		for i in range(0, self.rows):
			sum_matrix.matrix.append([])
			for j in range(0, self.cols):
				sum_matrix.matrix[i].append(self.matrix[i][j] + other.matrix[i][j])

		return sum_matrix


	def __sub__(self, other):
		if (self.rows != other.rows and self.cols != other.cols):
			return None

		if (self.matrix == None or other.matrix == None):
			return None

		diff_matrix = Matrix(self.rows, self.cols)
		for i in range(0, self.rows):
			diff_matrix.matrix.append([])
			for j in range(0, self.cols):
				diff_matrix.matrix[i].append(self.matrix[i][j] - other.matrix[i][j])

		return diff_matrix


	def __mul__(self, other):
		if (self.cols != other.rows):
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

