from Data_Structures.matrix.matrix import *

m = Matrix(3, 1, [1,2,3])
print m
print m.transpose()

m2 = Matrix(3, 2, [1,2,3,4,5,6])
print m2
print m2.transpose()

m3 = Matrix(4, 4, range(1, 17))
print m3
print m3.transpose()
