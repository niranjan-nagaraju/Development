from data_structures.heap.maxheap import MaxHeap
from data_structures.heap.heap import HeapEmptyError



def test_add():
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


def test_remove():
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



def test_build_heap():
	l = range(10, -1, -1)
	MaxHeap.build_heap(l)
	assert l == [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]



def test_increaseKey():
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
def test_sorted():
	l = range(10, -1, -1)
	MaxHeap.build_heap(l)
	h = MaxHeap()
	h.items = l

	new_l = []
	h.sorted(lambda i, l: l.append(i), new_l)

	# TODO: Fix assert fail here owing to bug in remove()
	# when bubble_down() is incorrect due to one/both of L-R not present
	assert new_l == range(11)


def test_isMaxHeap(recursive = True):
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
      1
     / \
    2   3
   / \
  4   5
	'''
	h.items = [1,2,3,4,5]
	assert isHeap() == True

	'''
      1
     /
    2
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



def test_custom_items():
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
	assert sorted_records_bs == range(1,6)

	h2 = MaxHeap(lambda r1, r2: cmp(r1.a, r2.a))
	h2.add(Record("record1", 1, 100))
	h2.add(Record("record4", 5, 125))
	h2.add(Record("record3", 2, 50))
	h2.add(Record("record2", 3, 25))
	h2.add(Record("record5", 4, 5))

	sorted_records_as = []
	h2.sorted(lambda x, l: l.append(x.a), sorted_records_as)
	assert sorted_records_as == ["record"+str(i) for i in xrange(1,6)]

	h3 = MaxHeap(lambda r1, r2: cmp(r1.c, r2.c))
	h3.add(Record("record1", 1, 100))
	h3.add(Record("record4", 5, 125))
	h3.add(Record("record3", 2, 50))
	h3.add(Record("record2", 3, 25))
	h3.add(Record("record5", 4, 5))

	sorted_records_cs = []
	h3.sorted(lambda x, l: l.append(x.c), sorted_records_cs)
	assert sorted_records_cs == sorted([100, 125, 50, 25, 5])



def test_custom_comparator():
	# Max heap using custom comparator
	h = MaxHeap(lambda a,b: cmp(b, a))
	for i in range(10):
		h.add(i)

	for i in range(9, -1, -1):
		assert h.remove() == i

def test_heap():
	#test_add()
	#test_remove()
	#test_build_heap()
	test_increaseKey()
	test_sorted()
	test_isHeap()
	test_isHeap(recursive=False)
	test_custom_items()
	test_custom_comparator()

if __name__ == '__main__':
	test_heap()


