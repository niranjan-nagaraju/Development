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
			self.comparatorfn = cmp


	# return the item at the front of the Queue
	# this is the highest priority item in the priority queue right now
	def peek(self):
		if not self.head:
			raise UnderFlowError
		return self.head.value


	# Add an item to the priority queue with an associated priority level
	def enqueue(self, item, priority):
		self.place((item, priority), lambda (a, p1),(b,p2): self.comparatorfn(p1, p2))



# basic priority queue testcases
def basic_testcases():
	pq = PriorityQueue()
	assert(pq.size == 0)

	pq.enqueue("Job1", 4)
	pq.enqueue("Job2", 3)
	pq.enqueue("Job3", 7)
	pq.enqueue("Job4", 1)
	pq.enqueue("Job5", 3)

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

	pq.enqueue("Job6", 3)
	pq.enqueue("Job7", 8)
	pq.enqueue("Job8", 0)
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
	pq = PriorityQueue(comparatorfn = lambda p1,p2: cmp(p2,p1))
	pq.enqueue("Job1", 4)
	pq.enqueue("Job2", 3)
	pq.enqueue("Job3", 7)
	pq.enqueue("Job4", 1)
	pq.enqueue("Job5", 3)

	assert str(pq) == "[5]: ('Job3', 7) ('Job1', 4) ('Job2', 3) ('Job5', 3) ('Job4', 1)"



if __name__ == '__main__':
	basic_testcases()



