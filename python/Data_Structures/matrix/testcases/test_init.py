from matrix import *

# Test matrix init and __str__ in print

m1 = Matrix(3, 2, [1,2,3,4,5,6])
print "From Flatlist: ", m1

m2 = Matrix(3, 2, [[1,2], [3,4], [5,6]])
print "From matrix: ", m2

m3 = Matrix()
print "Default No args: ", m3

m4 = Matrix(4, 5)
print "No matrix elements: ", m4

m5 = Matrix(4)
print "No matrix columns, Square empty matrix: ", m5

