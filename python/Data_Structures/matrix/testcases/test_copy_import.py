from matrix import *

# Test matrix copy and import from other structures(list, 2dlist)
a = Matrix(2,3, range(1,7))
b = a.copy()
assert(a == b)

# Now change b's rows -- shouldn't affect a
b.rows = 10
assert (a != b)

# Revert back and change matrix -- matrix list shouldn't be passed as a ref => deep copy
b.rows=3
assert (a==b)

b.matrix=range(1,7) # Flattened, original 'a' should have 2D list
assert (a != b)


# Test Import here


print 'All test Copy/Import testcases.... PASSED'
