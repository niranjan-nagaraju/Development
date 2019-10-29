'''
Implements segment tree - a RMQ (Range-Minimum-Query) data structure that supports
range queries in a list of numbers

Query f() across an interval, i..j
   e.g., f() can be sum of a[i..j], or min of a[i..j]

Update: updates a[i] to a new value x, and readjusts the segment tree so the RMQ is consistent with the update

The tree itself is represented as an array of size ~(2n-1) (where n is the size of the list) and
uses heap-like indexing to get to child and parent nodes
NOTE: Actual size of the segment tree would be 2S-1 where S is (next power of 2 after n=size of list)

e.g.
a: [1, 3, 5, 6, 4, 2]
idx 0  1  2  3  4  5
f(): Minimum

Building the Segment Tree
-------------------------
1 3 5             | 6 4 2
  1 3      | 5        6 4      | 2
    1 | 3               6 | 4

            1
	      (0,5) 
      /          \
     1            2
    (0,2)        (3,5)
    / \          /  \
   1   5        4    2 
 (0,1) (2,2)   (3,4) (5,5)
  / \          / \
 1   3        6   4
(0,0)(1,1)  (3,3) (4,4)

n = 6
=> ST size = 2*8-1 == 15
ST: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14
    1 1 2 1 5 4 2 1 3 -  -  6  4  -  -

ST[0]:  f(0,5)       = 1
ST[1]:  f(0,2)     = 1
ST[2]:  f(3,5)     = 2
ST[3]:  f(0,1) = 1
ST[4]:  f(2,2) = 5
ST[5]:  f(3,4)   = 4
ST[6]:  f(5,5) = 2
ST[7]:  f(0,0) = 1
ST[8]:  f(1,1) = 3
ST[9]:  -
ST[10]: -
ST[11]: f(3,3) = 6
ST[12]: f(4,4) = 4
ST[13]: -
ST[14]: -
'''
from math import ceil, log
class SegmentTree(object):
	def __init__(self, array):
		self._l = array
		next_2_power = 2**ceil(log(len(array), 2))
		self.tree = [None] * int(2 * next_2_power - 1)


	def construct(self, f):
		pass

	def query(self, f, i, j):
		pass

	def update(self, i, j , x):
		pass


if __name__ == '__main__':
	st = SegmentTree([1,3,5,6,4,2])
	assert len(st.tree) == 15

