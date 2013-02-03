from Data_Structures.matrix.matrix import *

m = Matrix(3, 1, [1,2,3])
assert (m.transpose() == Matrix(1, 3, [[1, 2, 3]]))
assert (m.transpose().transpose() == m) # Double transpose should give back the same matrix

m2 = Matrix(3, 2, [1,2,3,4,5,6])
assert (m2.transpose() == Matrix(2, 3, [[1, 3, 5], [2, 4, 6]]))
assert (m2.transpose().transpose() == m2) # Double transpose should give back the same matrix

m3 = Matrix(4, 4, range(1, 17))
assert (m3.transpose() == Matrix(4, [[1, 5, 9, 13], [2, 6, 10, 14], [3, 7, 11, 15], [4, 8, 12, 16]]))
assert (m3.transpose().transpose() == m3) # Double transpose should give back the same matrix

print 'All test transpose testcases.... PASSED'
