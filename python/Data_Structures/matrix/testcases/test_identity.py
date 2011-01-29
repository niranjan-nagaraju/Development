from Data_Structures.matrix.matrix import *

m1 = Matrix.getIdentityMatrix(3)
print m1

if m1.isIdentity():
	print "is Identity"
else:
	print "Is not an Identity"

m1 = Matrix(3, 3, [1,2,3,4,5,6,7,8,9])
print m1

if m1.isIdentity():
	print "is an Identity"
else:
	print "Is not an Identity"
