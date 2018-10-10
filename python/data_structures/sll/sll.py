from node import Node

# Exceptions for the SLL class
class OverFlowError(Exception):
	''' List Overflow '''
	pass

class UnderFlowError(Exception):
	''' List Underflow '''
	pass


# Iterator helper for SLL
class SLLIterator:
	def __init__(self, node=None):
		self.node = node

	def __iter__(self):
		return self


	def next(self):
		if not self.node:
			raise StopIteration
		else:
			tmp = self.node
			# Move iterator to next node for next iteration
			self.node = tmp.next
			return tmp.value


# The SLL class
class SLL:
	# A default print function if no aggregator is provided
	# for traversal functions
	_default_printfn = lambda x,y : sys.stdout.write(str(y))

	def __init__(self):
		self.head = None
		self.tail = None
		self.size = 0

		# function aliases for better readability
		# enqueue() and dequeue():
		#  Can be push_front/pop_back too, but usually u join at the back of q queue,
		#  and get processed at the front
		self.enqueue = self.push_back
		self.dequeue = self.pop_front
		self.push = self.push_back
		self.pop = self.pop_back


	def __str__(self):
		sll_str = '[' + str(self.size) + ']: '
		trav = self.head
		while (trav):
			sll_str += str(trav) + ' '
			trav = trav.next

		return sll_str



	def __repr__(self):
		return self.__str__()


	def __iter__(self):
		return SLLIterator(self.head)


	# Create a SLL from a list
	@staticmethod
	def fromList(lst):
		s = SLL()
		for x in lst:
			s.push_back(x)

		return s


	# Return a List constructed from SLL items
	@staticmethod
	def toList(sll):
		l = []
		for x in sll:
			l.append(x)

		return l



	# Traverse and print/aggregate SLL items using the aggregator function specified
	def traverse(self, aggregate_fn=_default_printfn, **kwargs):
		trav = self.head
		while (trav):
			aggregate_fn(kwargs, trav.value)
			trav = trav.next



	# Insert at front
	def push_front(self, value):
		node = Node(value)
		self.size += 1

		node.next = self.head
		self.head = node

		if (not self.tail):
			self.tail = node


	# Insert to the rear
	def push_back(self, value):
		node = Node(value)

		self.size += 1

		# No elements in the list
		if not self.tail:
			self.tail = node
			self.head = node
			return

		self.tail.next = node
		self.tail = node


	# remove first element from the SLL
	def pop_front(self):
		if not self.head:
			raise UnderFlowError
			return None

		self.size -= 1
		value = self.head.value
		self.head = self.head.next

		# We just popped the last element in the SLL,
		# Update tail
		if self.size == 0:
			self.tail = None

		return value


	# remove last element from the SLL	
	def pop_back(self):
		if not self.head:
			raise UnderFlowError
			return None

		self.size -= 1
		value = self.tail.value

		# There was only 1 element in the SLL
		if self.size == 0:
			self.head = self.tail = None
			return value

		trav = self.head
		# traverse until we reach the penultimate node in the SLL
		while trav.next != self.tail:
			trav = trav.next

		# make penultimate node the new tail, and cut old tail from its link
		trav.next = None
		self.tail = trav

		return value



	# Keep the SLL sorted, every insert places the item at the right place in order to keep the list sorted
	# NOTE: assumes the list is sorted - if all inserts are done using place() it actually would be
	#       A mixture of random push_xxx and place() will not ensure the list is sorted after place()
	#       When in doubt, sort the list first before calling place() for the new item.
	#
	# NOTE: the place() operation _is_ stable
	#    so if (a == b), and place(a) happened before place(b), then index(a) < index(b) in the list
	def place(self, item, comparatorfn=None):
		# if comparatorfn is not specified, try to use the item's __cmp__ method,
		# or the default __cmp__ if the item's class hasn't implemented one
		if not comparatorfn:
			comparatorfn = cmp

		# This is the first item to be 'placed' *OR* 
		# item < head.value => item is less than all elements in the current list,
		# Just add it to the list and return
		if not self.head or comparatorfn(item, self.head.value) < 0:
			self.push_front(item)
			return

		# if (item >= tail.value), append to the end and return
		# Will save traversing all the way to the end, 
		# if 'item' is bigger than all the elements in the current list
		if comparatorfn(item, self.tail.value) >= 0:
			self.push_back(item)
			return

		trav = self.head
		prev = None
		# Keep traversing until item < trav.value
		while trav and (comparatorfn(item, trav.value) >= 0):
			prev = trav
			trav = trav.next

		# At this point, we have found node trav, s.t
		# prev.value <= item < trav.value
		# Insert item between prev and trav
		node = Node(item)
		node.next = trav
		prev.next = node

		# Can't forget to update size
		self.size += 1


	# Reverse an SLL(iterative version)
	# Consider 3 nodes, x->y->z
	# a,b,c = x,y,z
	# Star with reversing a->b to a<-b,
	# then hop onto y,z,..., with a=y,b=z,... and repeat
	def reverse(self):
		if not self.head:
			return None

		a = self.head
		b = a.next
		while b:
			c = b.next

			# Make b->a link
			b.next = a

			a = b
			b = c

		# Mark erstwhile  head.next to None, so the SLL chain ends
		self.head.next = None 

		# When all's done, 'a' is pointing to the 'tail'
		# of the SLL
		# Since we have now reversed, point 'head' to 'a',
		# but before that, Update 'tail' to erstwhile 'head'
		self.tail = self.head
		self.head = a



def TC1():
	sll = SLL()
	sll.push_back(1)
	assert(sll.size == 1)
	assert(sll.head.value == 1) 
	assert(sll.tail.value == 1) 

	sll.push_front(0)
	assert(sll.size == 2)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 1) 


	sll.push_back(2)
	assert(sll.size == 3)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 2) 

	sll.enqueue(3)
	assert(sll.size == 4)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 3) 

	sll.push(4)
	assert(sll.size == 5)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 4) 

	assert(4 == sll.pop_back())
	assert(sll.size == 4)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 3) 

	assert(0 == sll.pop_front())
	assert(sll.size == 3)
	assert(sll.head.value == 1) 
	assert(sll.tail.value == 3) 

	assert(1 == sll.dequeue())
	assert(sll.size == 2)
	assert(sll.head.value == 2) 
	assert(sll.tail.value == 3) 

	assert(3 == sll.pop())
	assert(sll.size == 1)
	assert(sll.head.value == 2) 
	assert(sll.tail.value == 2) 


	assert(2 == sll.pop_back())
	assert(sll.size == 0)
	assert(sll.head == None) 
	assert(sll.tail == None) 


	# fromList and toList
	assert(SLL.toList(SLL.fromList(range(9, 0, -2))) == range(9, 0, -2))



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



def test_iterator():
	s = SLL()
	for i in range(10):
		s.push_back(i+1)

	it = iter(s)
	i = 1
	for x in it:
		assert(x == i)
		i = i + 1

	it = iter(s)
	i = 1
	while it:
		try:
			assert(it.next() == i)
		except StopIteration:
			break
		i = i + 1


	it = iter(s)
	i = 1
	while it:
		try:
			assert(next(it) == i)
		except StopIteration:
			break
		i = i + 1


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


if __name__ == "__main__":
	TC1()
	test_place()
	test_iterator()
	test_reverse()
	print 'Testcases passed'

