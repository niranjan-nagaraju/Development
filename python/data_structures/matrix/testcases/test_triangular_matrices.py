from matrix import *

'''
Test Lower and Upper Triangular matrices
'''

m = Matrix(3, range(1,10))
assert (m.upper_triangle() == Matrix(3, [[1, 2, 3], [0, 5, 6], [0, 0, 9]]))
assert (m.lower_triangle() == Matrix(3, [[1, 0, 0], [4, 5, 0], [7, 8, 9]]))

assert (Matrix(3,4, range(1, 13)).upper_triangle() == None)
assert (Matrix(3,4, range(1, 13)).lower_triangle() == None)

print 'All test triangular matrices testcases.... PASSED'


