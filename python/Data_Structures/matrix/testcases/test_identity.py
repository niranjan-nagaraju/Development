from Data_Structures.matrix.matrix import *

# Testcases for Identity operations

m1 = Matrix.getIdentityMatrix(3)
assert(m1 == Matrix(3, [[1,0,0],[0,1,0],[0,0,1]]))
assert(m1.isIdentity() == True)


m1 = Matrix(3, 3, [1,2,3,4,5,6,7,8,9])
assert(m1.isIdentity() == False)

# Multiply with identity matrix and verify if the identity matrix is indeed the identity matrix ;)
assert (m1 * Matrix.getIdentityMatrix(3) == m1)
assert (Matrix.getIdentityMatrix(3) * m1 == m1)


print 'All test Identity Matrix testcases.... PASSED'
