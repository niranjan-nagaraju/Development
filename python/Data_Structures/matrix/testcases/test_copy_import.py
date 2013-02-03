from matrix import *

# Test matrix copy and import from other structures(list, 2dlist)
a = Matrix(2,3, range(1,7))

b = a.copy()
assert(a == b)

# Now change b's rows -- shouldn't affect a
b.rows = 10
assert (a != b)

# Revert back and change matrix -- matrix list shouldn't be passed as a ref => deep copy
b.rows=2
assert (a==b)

b.matrix=range(1,7) # Flattened, original 'a' should have 2D list
assert (a != b)

# Test Import here
a = Matrix(2, 3)
a.fromList(range(1,7)) # 1D list

assert(a == Matrix(2, 3, [[1,2,3],[4,5,6]]))

a = Matrix(3) # 2D list
a.fromList([ [i+j for j in range(1,4)] for i in range(4) ]) # [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

print 'All test Copy/Import testcases.... PASSED'
