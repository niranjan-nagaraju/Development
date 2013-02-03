from matrix import *

# Test matrix equality '==' operator 

a = Matrix(2, 2, range(1,5))
b = Matrix(rows=2, matrix=[[1,2],[3,4]])

c = Matrix(2, 2, [5,6,7,8]) 
c.matrix = [[1,2],[3,4]] # Manually change contents

assert(a == b)
assert(b == c)

d = Matrix(2,3,range(1,7))
e = Matrix(2,3)
e.Matrix = range(2,8)
assert(d != e)

d = Matrix(2,3,range(1,7))
e = Matrix(2,3,range(1,7))
#Manually change e's cols 
e.cols = 2
assert(d != e)


print 'All test equality testcases.... PASSED'
