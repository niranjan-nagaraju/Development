from data_structures.heap.heap import Heap, HeapEmptyError



def test_add():
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
		assert h.items == intermediate_heaps[i]


def test_remove():
	l = [5,4,3,2,1,6,7,8,9,0]
	h = Heap()

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
	for i in xrange(10):
		item = h.remove()
		assert (len(h) == 10-i-1)
		assert item == i



def test_build_heap():
	l = range(10, -1, -1)
	Heap.build_heap(l)
	assert l == [0, 1, 4, 2, 6, 5, 8, 3, 7, 9, 10]



def test_decreaseKey():
	l = [0, 1, 4, 2, 6, 5, 8, 3, 7, 9, 10]
	h = Heap()
	h.items = l

	caught_exception = False
	try:
		h.decreaseKey(4, 7)
	except ValueError as v:
		assert v.message == "ValueError: decreaseKey() - New key should be less current value"
		caught_exception = True
	assert caught_exception == True

	h.decreaseKey(4, 0)
	assert h.items == [0, 0, 4, 2, 1, 5, 8, 3, 7, 9, 10]


def test_sorted():
	l = range(10, -1, -1)
	Heap.build_heap(l)
	h = Heap()
	h.items = l

	new_l = []
	h.sorted(lambda i, l: l.append(i), new_l)

	# TODO: Fix assert fail here owing to bug in remove()
	# when bubble_down() is incorrect due to one/both of L-R not present
	assert new_l == range(11)


def basic_testcases():
	test_add()
	test_remove()
	test_build_heap()
	test_decreaseKey()
	test_sorted()

if __name__ == '__main__':
	basic_testcases()


