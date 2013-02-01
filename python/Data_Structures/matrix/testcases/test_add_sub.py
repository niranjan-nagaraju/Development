from matrix import *

a = Matrix(2, 3, range(1,7))
b = Matrix(2, 3, range(2,8))

print 'a:', a
print 'b:', b

print 'sum:', a+b

c = Matrix(2,4, range(1,9))
print 'summing'
print a
print c
print a+c
