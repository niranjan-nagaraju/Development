from data_structures.sll.sll import SLL, UnderFlowError

'''
Test the plac() operation
'''
def test_place():
	s = SLL()
	for x in [6,4,2]:
		s.place(x)

	for x in [5, 3, 1]:
		s.place(x)

	s.place(0)
	s.place(7)

	l = []
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data)
	s.traverse(collate_fn, lst=l)
	assert(l == range(8))

	# Test custom comparator
	s2 = SLL()
	test_list = [(2,1), (1, 'a'), (2, 3), (0, 'c'), (4,1), (3,0)]
	for x in test_list:
		s2.place(x, lambda (a,b),(c,d): cmp(a, c))

	l = []
	collate_fn = lambda xargs, data : xargs['lst'].append(data)
	s2.traverse(collate_fn, lst=l)
	assert(l == [(0, 'c'), (1, 'a'), (2, 1), (2, 3), (3, 0), (4, 1)])



'''
Test SLL iterator support
'''
def test_iterator():
	s = SLL()
	for i in range(10):
		s.push_back(i+1)

	# Using for/in -> implicit next()
	it = iter(s)
	i = 1
	for x in it:
		assert(x == i)
		i = i + 1


	# Using the next/iter methods
	it = iter(s)
	i = 1
	while it:
		try:
			assert(it.next() == i)
		except StopIteration:
			break
		i = i + 1


	# Use an external next() on the iterator as opposed to it.next()
	it = iter(s)
	i = 1
	while it:
		try:
			assert(next(it) == i)
		except StopIteration:
			break
		i = i + 1


'''
Test SLL reverse() function
'''
def test_reverse():
	sll = SLL()
	sll.push_back(1)
	sll.push_back(2)
	sll.push_back(3)
	sll.push_back(4)

	sll.reverse()

	it = iter(sll)
	i = 4
	for x in it:
		assert(x == i)
		i -= 1


	# sll itself returns an SLLIterator, so we needn't get an iterator to
	# traverse the sll
	i = 4
	for x in sll:
		assert(x == i)
		i -= 1

	s2 = SLL()
	s2.push_back('a')
	s2.reverse()
	assert(s2.size == 1)
	assert(s2.head.value == 'a')
	assert(s2.tail.value == 'a')

	s2.push_back('b')
	s2.reverse()
	assert(s2.size == 2)
	assert(s2.head.value == 'b')
	assert(s2.tail.value == 'a')


'''
Index [] operator test
'''
def test_indexing():
	sll = SLL.fromList(range(1,11))

	for i in range(10):
		assert(sll[i] == i+1)
		
	try:
		assert(sll[10] == None)
	except IndexError:
		print "sll[10] index error on sll, SLL has only %d elements" %(len(sll))

	for i in range(1,11):
		assert(sll[-i] == 10-i+1)

	try:
		assert(sll[-11] == None)
	except IndexError:
		print "sll[-11] index error on sll, SLL has only %d elements" %(len(sll))



if __name__ == "__main__":
	test_place()
	test_iterator()
	test_reverse()
	test_indexing()
	print 'SLL Testcases passed'

