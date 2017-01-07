from matrix import *

# Test matrix empty()

a = Matrix(2, 2) # Triggers TypeError
assert (a.empty() == True)

a = Matrix(2, 2, []) # Should raise IndexError
assert (a.empty() == True)

a = Matrix(2, 2, [1,2,3,4])
assert (a.empty() == False)

# Improper number of elements passed to the matrix -- should be empty
a = Matrix(2, 2, [1,2,3,4,5])
assert (a.rows == 0)
assert (a.cols == 0)
assert (a.empty() == True)

# Manual assignment, not empty - though invalid
a = Matrix()
a.matrix=[1,2,3,4]
assert (a.empty() == False)

print 'All test empty testcases.... PASSED'
