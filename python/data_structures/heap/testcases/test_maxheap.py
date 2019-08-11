from data_structures.heap.maxheap import MaxHeap
from data_structures.heap.heap import HeapEmptyError
import unittest


class TestHeap(unittest.TestCase):
	def test_add(self):
		l = [5,4,3,2,1,6,7,8,9,0]
		intermediate_heaps = [
				[5],
				[5, 4],
				[5, 4, 3],
				[5, 4, 3, 2],
				[5, 4, 3, 2, 1],
				[6, 4, 5, 2, 1, 3],
				[7, 4, 6, 2, 1, 3, 5],
				[8, 7, 6, 4, 1, 3, 5, 2],
				[9, 8, 6, 7, 1, 3, 5, 2, 4],
				[9, 8, 6, 7, 1, 3, 5, 2, 4, 0]
				]

		h = MaxHeap()
		for i in xrange(len(l)):
			h.add(l[i])
			assert h.items == intermediate_heaps[i]


	def test_remove(self):
		l = [5,4,3,2,1,6,7,8,9,0]
		h = MaxHeap()

		caught_exception = False
		try:
			assert h.peek() == None
		except HeapEmptyError as e:
			assert str(e) == "HeapEmptyError: 'peek(): Heap is empty'"
			caught_exception = True
		assert caught_exception == True


		caught_exception = False
		try:
			assert h.remove() == None
		except HeapEmptyError as e:
			assert str(e) == "HeapEmptyError: 'remove(): Heap is empty'"
			caught_exception = True
		assert caught_exception == True


		for i in xrange(len(l)):
			h.add(l[i])

		assert len(h) == 10
		for i in xrange(9, -1, -1):
			item = h.remove()
			assert (len(h) == i)
			assert item == i



	def test_build_heap(self):
		l = range(10, -1, -1)
		MaxHeap.build_heap(l)
		assert l == range(10, -1, -1)

		l = range(11)
		MaxHeap.build_heap(l)
		assert l == [10, 9, 6, 8, 4, 5, 2, 7, 3, 1, 0]


	def test_increaseKey(self):
		l = [0, 1, 4, 2, 6, 5, 8, 3, 7, 9, 10]
		h = MaxHeap()

		for x in l:
			h.add(x)

		assert h.items == [10, 9, 6, 4, 8, 1, 5, 0, 3, 2, 7]

		caught_exception = False
		try:
			h.increaseKey(4, 5)
		except ValueError as v:
			assert v.message == "ValueError: increaseKey() - New key should be greater than current value"
			caught_exception = True
		assert caught_exception == True

		h.increaseKey(4, 11)
		assert h.items == [11, 10, 6, 4, 9, 1, 5, 0, 3, 2, 7]


	# TODO: Needs fix from here on
	def test_sorted(self):
		l = range(10, -1, -1)
		MaxHeap.build_heap(l)
		h = MaxHeap()
		h.items = l

		new_l = []
		h.sorted(lambda i, l: l.append(i), new_l)

		assert new_l == range(10, -1, -1)


	def test_isMaxHeap(self, recursive = True):
		h = MaxHeap()
		if recursive:
			isHeap = h.isHeap
		else:
			isHeap = h.isHeap_i

		h.items = []
		assert isHeap() == True

		h.items = [1, 1]
		assert isHeap() == True

		h.items = [1, 1, 1]
		assert isHeap() == True

		'''
		  5
		 / \
		4   3
	   / \
	  2   1
		'''
		h.items = [5,4,3,2,1]
		assert isHeap() == True

		'''
		  2
		 /
		1
		'''
		h.items = [2, 1]
		assert isHeap() == True

		h.items = [1, 2]
		assert isHeap() == False

		'''
		  3
		 / \
		1   2
		'''
		h.items = [3, 1, 2]
		assert isHeap() == True

		'''
		   1
		 /   \
		2     3
	   / \  
	  4   5 
		'''
		h.items = [1, 2, 3, 4, 5]
		assert isHeap() == False

		h.items = range(10, -1, -1)
		assert isHeap() == True

		h.items = range(10)
		assert isHeap() == False



	def test_custom_items(self):
		class Record:
			def __init__(self, a, b, c):
				self.a = a
				self.b = b
				self.c = c

			# by default, using 'b' as key to compare
			def __cmp__(self, other):
				return cmp(self.b, other.b)

			def __str__(self):
				return "(%s, %s, %s)" %(self.a, self.b, self.c)

		h = MaxHeap()
		h.add(Record("record1", 1, 100))
		h.add(Record("record4", 5, 125))
		h.add(Record("record3", 2, 50))
		h.add(Record("record2", 3, 25))
		h.add(Record("record5", 4, 5))

		sorted_records_bs = []
		h.sorted(lambda x, l: l.append(x.b), sorted_records_bs)
		assert sorted_records_bs == range(5, 0, -1)

		h2 = MaxHeap(lambda r1, r2: cmp(r1.a, r2.a))
		h2.add(Record("record1", 1, 100))
		h2.add(Record("record4", 5, 125))
		h2.add(Record("record3", 2, 50))
		h2.add(Record("record2", 3, 25))
		h2.add(Record("record5", 4, 5))

		sorted_records_as = []
		h2.sorted(lambda x, l: l.append(x.a), sorted_records_as)
		assert sorted_records_as == ["record"+str(i) for i in xrange(5, 0, -1)]

		h3 = MaxHeap(lambda r1, r2: cmp(r1.c, r2.c))
		h3.add(Record("record1", 1, 100))
		h3.add(Record("record4", 5, 125))
		h3.add(Record("record3", 2, 50))
		h3.add(Record("record2", 3, 25))
		h3.add(Record("record5", 4, 5))

		sorted_records_cs = []
		h3.sorted(lambda x, l: l.append(x.c), sorted_records_cs)
		assert sorted_records_cs == sorted([100, 125, 50, 25, 5])[::-1]



	def test_custom_comparator(self):
		# Max heap using custom comparator making it a minheap
		h = MaxHeap(lambda a,b: cmp(b, a))
		for i in range(10):
			h.add(i)

		assert h.items == range(10)
		for i in range(10):
			assert h.remove() == i


	def test_sort(self):
		assert MaxHeap.sort([1,3,5,6,4,2]) == range(1, 7)



if __name__ == '__main__':
	unittest.main()


