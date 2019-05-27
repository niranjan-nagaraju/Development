from data_structures.heap.heap import Heap, HeapEmptyError
import unittest



class TestHeap(unittest.TestCase):
	def test_add(self):
		l = [5,4,3,2,1,6,7,8,9,0]
		intermediate_heaps = [
				[5],
				[4, 5],
				[3, 5, 4],
				[2, 3, 4, 5],
				[1, 2, 4, 5, 3],
				[1, 2, 4, 5, 3, 6],
				[1, 2, 4, 5, 3, 6, 7],
				[1, 2, 4, 5, 3, 6, 7, 8],
				[1, 2, 4, 5, 3, 6, 7, 8, 9],
				[0, 1, 4, 5, 2, 6, 7, 8, 9, 3]
				]

		h = Heap()
		for i in xrange(len(l)):
			h.add(l[i])
			self.assertEqual (h.items, intermediate_heaps[i])


	def test_remove(self):
		l = [5,4,3,2,1,6,7,8,9,0]
		h = Heap()

		with self.assertRaises(HeapEmptyError):
			h.peek()

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
		for i in xrange(10):
			item = h.remove()
			self.assertEqual(len(h), 10-i-1)
			self.assertEqual(item, i)



	def test_build_heap(self):
		l = range(10, -1, -1)
		Heap.build_heap(l)
		self.assertEqual(l, [0, 1, 4, 2, 6, 5, 8, 3, 7, 9, 10])

		l = range(11)
		Heap.build_heap(l)
		assert l == range(11)



	def test_decreaseKey(self):
		l = [0, 1, 4, 2, 6, 5, 8, 3, 7, 9, 10]
		h = Heap()
		h.items = l

		caught_exception = False
		try:
			h.decreaseKey(4, 7)
		except ValueError as v:
			assert v.message == "ValueError: decreaseKey() - New key should be less than current value"
			caught_exception = True
		assert caught_exception == True

		h.decreaseKey(4, 0)
		assert h.items == [0, 0, 4, 2, 1, 5, 8, 3, 7, 9, 10]


	def test_sorted(self):
		l = range(10, -1, -1)
		Heap.build_heap(l)
		h = Heap()
		h.items = l

		new_l = []
		h.sorted(lambda i, l: l.append(i), new_l)

		# TODO: Fix assert fail here owing to bug in remove()
		# when bubble_down() is incorrect due to one/both of L-R not present
		assert new_l == range(11)


	def test_isHeap(self, recursive = True):
		h = Heap()
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
		  1
		 / \
		2   3
	   / \
	  4   5
		'''
		h.items = [1,2,3,4,5]
		assert isHeap() == True

		'''
		  2
		 /
		1
		'''
		h.items = [2, 1]
		assert isHeap() == False

		'''
		  2
		 / \
		3   1
		'''
		h.items = [2, 3, 1]
		assert isHeap() == False

		'''
		   1
		 /   \
		2     3
	   / \   /
	  4   5 1
		'''
		h.items = [1, 2, 3, 4, 5, 1]
		assert isHeap() == False

		h.items = range(10, -1, -1)
		assert isHeap() == False

		h.items = range(10)
		assert isHeap() == True



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

		h = Heap()
		h.add(Record("record1", 1, 100))
		h.add(Record("record4", 5, 125))
		h.add(Record("record3", 2, 50))
		h.add(Record("record2", 3, 25))
		h.add(Record("record5", 4, 5))

		sorted_records_bs = []
		h.sorted(lambda x, l: l.append(x.b), sorted_records_bs)
		assert sorted_records_bs == range(1,6)

		h2 = Heap(lambda r1, r2: cmp(r1.a, r2.a))
		h2.add(Record("record1", 1, 100))
		h2.add(Record("record4", 5, 125))
		h2.add(Record("record3", 2, 50))
		h2.add(Record("record2", 3, 25))
		h2.add(Record("record5", 4, 5))

		sorted_records_as = []
		h2.sorted(lambda x, l: l.append(x.a), sorted_records_as)
		assert sorted_records_as == ["record"+str(i) for i in xrange(1,6)]

		h3 = Heap(lambda r1, r2: cmp(r1.c, r2.c))
		h3.add(Record("record1", 1, 100))
		h3.add(Record("record4", 5, 125))
		h3.add(Record("record3", 2, 50))
		h3.add(Record("record2", 3, 25))
		h3.add(Record("record5", 4, 5))

		sorted_records_cs = []
		h3.sorted(lambda x, l: l.append(x.c), sorted_records_cs)
		assert sorted_records_cs == sorted([100, 125, 50, 25, 5])



	def test_custom_comparator(self):
		# Max heap using custom comparator
		h = Heap(lambda a,b: cmp(b, a))
		for i in range(10):
			h.add(i)

		for i in range(9, -1, -1):
			assert h.remove() == i


if __name__ == '__main__':
	unittest.main()


