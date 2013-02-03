from matrix import *

# Test Matrix multiplications (scalar and vector products)

# Dot products
m1 = Matrix(3, range(1,10))
m2 = m1 * 3
assert (m2 == Matrix(3, range(3, 28,  3)))

m1 = Matrix(2, 3, range(1,7))
m2 = m1 * 4
assert (m2 == Matrix(2, 3, range(4, 28,  4)))

m1 = Matrix(2,3)
m2 = m1*3
assert (m2 == None)


# Vector Products
m1 = Matrix(3, range(1, 10))
m2 = Matrix(3, range(9, 0, -1))

# Shamelessly Verified by an online matrix multiplier @
# http://www.bluebit.gr/matrix-calculator/multiply.aspx
assert (m1*m2 == Matrix(3, [[30, 24, 18],[84, 69, 54],[138, 114, 90]]))


m1 = Matrix(3,4, range(1, 13))
assert (m1*m1 == None) # Can't multiply 3x4 with 3x4

m2 = Matrix(4,3, range(1, 13))

# Shamelessly Verified by an online matrix multiplier @
# http://www.bluebit.gr/matrix-calculator/multiply.aspx
assert(m1*m2 == Matrix(3, 3, [[70, 80, 90], [158, 184, 210], [246, 288, 330]]))

print 'All test multiplication testcases.... PASSED'
