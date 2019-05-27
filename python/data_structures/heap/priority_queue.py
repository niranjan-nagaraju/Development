from data_structures.heap.heap import Heap, HeapEmptyError


# Priority Queue using a Heap
class PriorityQueue(Heap):
	def __init__(self, comparatorfn=None):
		Heap.__init__(self, comparatorfn)
		self.dequeue = self.remove


	# Add an item to the priority queue with an associated priority level
	def enqueue(self, item):
		self.add(item)



# basic priority queue testcases
def basic_testcases():
	pq = PriorityQueue()
	pq.enqueue(4)
	pq.enqueue(5)
	pq.enqueue(1)
	pq.enqueue(3)
	pq.enqueue(2)
	assert len(pq) == 5
	assert str(pq) == "[5]: 1 2 4 5 3"

	pq = PriorityQueue(lambda (a,p1),(b,p2): cmp(p1,p2))
	assert(len(pq) == 0)

	pq.enqueue(("Job1", 4))
	pq.enqueue(("Job2", 3))
	pq.enqueue(("Job3", 7))
	pq.enqueue(("Job4", 1))
	pq.enqueue(("Job5", 3))

	assert pq.items == [('Job4', 1), ('Job2', 3), ('Job3', 7), ('Job1', 4), ('Job5', 3)]
	assert pq.peek() == ('Job4', 1)
	assert len(pq) == 5
	assert pq.dequeue() == ('Job4', 1)

	assert pq.peek() == ('Job2', 3)
	assert len(pq) == 4
	assert pq.dequeue() == ('Job2', 3)

	assert pq.items == [('Job5', 3), ('Job1', 4), ('Job3', 7)] # re-org heap
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

	assert pq.items == [('Job3', 7), ('Job2', 3), ('Job1', 4), ('Job4', 1), ('Job5', 3)]

	# Use default cmp(), in a pair (a,b), (c,d)
	# (a,b) < (c,d) if a<b
	pq = PriorityQueue()
	pq.enqueue(("Job4", 4))
	pq.enqueue(("Job3", 3))
	pq.enqueue(("Job7", 7))
	pq.enqueue(("Job1", 1))
	pq.enqueue(("Job3", 11))

	assert pq.items == [('Job1', 1), ('Job3', 3), ('Job7', 7), ('Job4', 4), ('Job3', 11)]


if __name__ == '__main__':
	basic_testcases()


