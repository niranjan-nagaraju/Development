from matrix import *

# Test matrix init and __str__ in print

m1 = Matrix(3, 2, [[1,2], [3,4], [5,6]])
assert (m1.rows == 3)
assert (m1.cols == 2)
assert (m1.matrix == [[1,2],[3,4],[5,6]])

m2 = Matrix(2, 3, [1,2,3,4,5,6])
assert (m2.rows == 2)
assert (m2.cols == 3)
assert (m2.matrix == [[1,2,3],[4,5,6]])

m3 = Matrix()
assert (m3.rows == 0)
assert (m3.cols == 0)
assert (m3.matrix == [])

# Empty matrix => empty matrix
m4 = Matrix(4, 5)
assert (m4.rows == 0)
assert (m4.cols == 0)
assert (m4.matrix == [])

m5 = Matrix(4, matrix=range(1, 17))
assert (m5.rows == 4)
assert (m5.cols == 4)
assert (m5.matrix == [range(i,i+4) for i in range(1, 17, 4)])

m6 = Matrix(4, matrix=(1,2)) # Tuple import not supported => Empty matrix
assert (m6.rows == 0)
assert (m6.cols == 0)
assert (not m6.matrix)

m7 = Matrix(4, matrix=1.04)  # Should trigger TypeError
assert (m7.rows == 0)
assert (m7.cols == 0)
assert (not m7.matrix)


print 'All Init testcases.... PASSED'
