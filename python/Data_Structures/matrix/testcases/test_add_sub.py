from matrix import *

# testcases for matrix addition
a = Matrix(2, 3, range(1,7))
b = Matrix(2, 3, range(2,8))
assert(a+b == Matrix(2, 3, range(3, 15, 2)))

c = Matrix(2,4, range(1,9))
assert(a+c == None)

# Testcases for matrix subtraction
assert (b-a == Matrix(2,3, [1]*6))
assert(a-c == None)
assert(c-a == None)

print 'All test add/sub testcases.... PASSED'
