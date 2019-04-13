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


def basic_testcases():
	test_add()
	test_remove()

if __name__ == '__main__':
	basic_testcases()

