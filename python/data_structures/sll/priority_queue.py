'''
SLL based priority queue implementation.
Keep priority queue sorted by priority using SLL place()

enqueue(item, pri):
	Uses place() to add item in the right place and keep the priority queue sorted.

dequeue():
	Dequeue from the front of the queue.

peek():
	Next item by priority -> front of the queue.
'''

from sll import SLL
from data_structures.sll.sll import UnderFlowError

# Priority Queue using a SLL
class PriorityQueue(SLL):
	def __init__(self, comparatorfn=None):
		SLL.__init__(self)

		# Meaningful aliases for enqueue and dequeue
		# for queue operations
		self.dequeue = self.pop_front
		if comparatorfn:
			self.comparatorfn = comparatorfn
		else:
			# if no comparison is provided
			# try using standard cmp()
			self.comparatorfn = cmp


	# return the item at the front of the Queue
	# this is the highest priority item in the priority queue right now
	def peek(self):
		if not self.head:
			raise UnderFlowError
		return self.head.value


	# Add an item to the priority queue with an associated priority level
	def enqueue(self, item):
		self.place(item, self.comparatorfn)


	# Increase priority of item based on 'new' value
	# new should be < current
	def promote(self, item, new):
		node = self.remove(item)
		self.place(new, self.comparatorfn)


	# Decrease priority of item based on 'new' value
	# new should be > current
	def demote(self, item, new):
		node = self.remove(item)
		self.place(new, self.comparatorfn)



# basic priority queue testcases set #1
def basic_testcases1():
	pq = PriorityQueue()
	pq.enqueue(4)
	pq.enqueue(5)
	pq.enqueue(1)
	pq.enqueue(3)
	pq.enqueue(2)
	assert len(pq) == 5

	pq = PriorityQueue(lambda (a,p1),(b,p2): cmp(p1,p2))
	assert(len(pq) == 0)

	pq.enqueue(("Job1", 4))
	pq.enqueue(("Job2", 3))
	pq.enqueue(("Job3", 7))
	pq.enqueue(("Job4", 1))
	pq.enqueue(("Job5", 3))

	assert pq.toList() == [('Job4', 1), ('Job2', 3), ('Job5', 3), ('Job1', 4), ('Job3', 7)]
	assert pq.peek() == ('Job4', 1)
	assert len(pq) == 5
	assert pq.dequeue() == ('Job4', 1)

	assert pq.peek() == ('Job2', 3)
	assert len(pq) == 4
	assert pq.dequeue() == ('Job2', 3)

	assert pq.toList() == [('Job5', 3), ('Job1', 4), ('Job3', 7)]
	assert pq.peek() == ('Job5', 3)
	assert len(pq) == 3
	assert pq.dequeue() == ('Job5', 3)
	assert len(pq) == 2

	pq.enqueue(("Job6", 3))
	pq.enqueue(("Job7", 8))
	pq.enqueue(("Job8", 0))
	assert len(pq) == 5
	assert pq.peek() == ('Job8', 0)

	assert pq.peek() == ('Job8', 0)
	assert pq.dequeue() == ('Job8', 0)
	assert len(pq) == 4

	assert pq.peek() == ('Job6', 3)
	assert pq.dequeue() == ('Job6', 3)
	assert len(pq) == 3

	assert pq.peek() == ('Job1', 4)
	assert pq.dequeue() == ('Job1', 4)
	assert len(pq) == 2

	assert pq.peek() == ('Job3', 7)
	assert pq.dequeue() == ('Job3', 7)
	assert len(pq) == 1

	assert pq.peek() == ('Job7', 8)
	assert pq.dequeue() == ('Job7', 8)
	assert len(pq) == 0
	assert (not pq == True)

	# priority queue but higher value => higher priority
	pq = PriorityQueue(comparatorfn = lambda (a,p1),(b,p2): cmp(p2,p1))
	pq.enqueue(("Job1", 4))
	pq.enqueue(("Job2", 3))
	pq.enqueue(("Job3", 7))
	pq.enqueue(("Job4", 1))
	pq.enqueue(("Job5", 3))

	assert pq.toList() == [('Job3', 7), ('Job1', 4), ('Job2', 3), ('Job5', 3), ('Job4', 1)]

	# Lower value => Higher priority
	pq = PriorityQueue(comparatorfn = lambda (a,p1),(b,p2): cmp(p1,p2))
	pq.enqueue(("Job4", 4))
	pq.enqueue(("Job3", 3))
	pq.enqueue(("Job7", 7))
	pq.enqueue(("Job1", 1))
	pq.enqueue(("JobX", 11))

	assert pq.toList() == [('Job1', 1), ('Job3', 3), ('Job4', 4), ('Job7', 7), ('JobX', 11)]
	pq.promote(('JobX', 11), ('JobX', 1))

	assert pq.toList() == [('Job1', 1), ('JobX', 1), ('Job3', 3), ('Job4', 4), ('Job7', 7)]
	pq.demote(('JobX', 1), ('JobX', 2))

	assert pq.toList() == [('Job1', 1), ('JobX', 2), ('Job3', 3), ('Job4', 4), ('Job7', 7)]
	pq.demote(('JobX', 2), ('JobX', 6))
	assert pq.toList() == [('Job1', 1), ('Job3', 3), ('Job4', 4), ('JobX', 6), ('Job7', 7)]




# basic priority queue testcases set #2
def basic_testcases2():
	pq = PriorityQueue()
	pq.enqueue(4)
	pq.enqueue(5)
	pq.enqueue(1)
	pq.enqueue(3)
	pq.enqueue(2)
	assert pq.size == 5
	assert str(pq) == "[5]: 1 2 3 4 5"

	pq = PriorityQueue(lambda (a,p1),(b,p2): cmp(p1,p2))
	assert(pq.size == 0)

	pq.enqueue(("Job1", 4))
	pq.enqueue(("Job2", 3))
	pq.enqueue(("Job3", 7))
	pq.enqueue(("Job4", 1))
	pq.enqueue(("Job5", 3))

	assert str(pq) == "[5]: ('Job4', 1) ('Job2', 3) ('Job5', 3) ('Job1', 4) ('Job3', 7)"
	assert pq.peek() == ('Job4', 1)
	assert pq.size == 5
	assert pq.dequeue() == ('Job4', 1)

	assert pq.peek() == ('Job2', 3)
	assert pq.size == 4
	assert pq.dequeue() == ('Job2', 3)

	assert pq.peek() == ('Job5', 3)
	assert pq.size == 3
	assert pq.dequeue() == ('Job5', 3)
	assert pq.size == 2

	pq.enqueue(("Job6", 3))
	pq.enqueue(("Job7", 8))
	pq.enqueue(("Job8", 0))
	assert pq.size == 5
	assert pq.peek() == ('Job8', 0)

	assert pq.peek() == ('Job8', 0)
	assert pq.dequeue() == ('Job8', 0)
	assert pq.size == 4

	assert pq.peek() == ('Job6', 3)
	assert pq.dequeue() == ('Job6', 3)
	assert pq.size == 3

	assert pq.peek() == ('Job1', 4)
	assert pq.dequeue() == ('Job1', 4)
	assert pq.size == 2

	assert pq.peek() == ('Job3', 7)
	assert pq.dequeue() == ('Job3', 7)
	assert pq.size == 1

	assert pq.peek() == ('Job7', 8)
	assert pq.dequeue() == ('Job7', 8)
	assert pq.size == 0
	assert (not pq == True)

	# priority queue but higher value => higher priority
	pq = PriorityQueue(comparatorfn = lambda (a,p1),(b,p2): cmp(p2,p1))
	pq.enqueue(("Job1", 4))
	pq.enqueue(("Job2", 3))
	pq.enqueue(("Job3", 7))
	pq.enqueue(("Job4", 1))
	pq.enqueue(("Job5", 3))

	assert str(pq) == "[5]: ('Job3', 7) ('Job1', 4) ('Job2', 3) ('Job5', 3) ('Job4', 1)"

	# Use default cmp(), in a pair (a,b), (c,d)
	# (a,b) < (c,d) if a<b
	pq = PriorityQueue()
	pq.enqueue(("Job4", 4))
	pq.enqueue(("Job3", 3))
	pq.enqueue(("Job7", 7))
	pq.enqueue(("Job1", 1))
	pq.enqueue(("Job3", 11))

	assert str(pq) == "[5]: ('Job1', 1) ('Job3', 3) ('Job3', 11) ('Job4', 4) ('Job7', 7)"


if __name__ == '__main__':
	basic_testcases1()
	basic_testcases2()



