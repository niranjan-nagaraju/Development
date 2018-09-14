from sll import SLL

# Queue using a SLL
class Queue:
	def __init__(self):
		self.sll = SLL()

	def size(self):
		return self.sll.size

	# enqueue element into the back of the queue
	def enqueue(self, value):
		self.sll.enqueue(value)


	# dequeue element from the front of the queue and return it
	def dequeue(self):
		return self.sll.dequeue()


	def __str__(self):
		return self.sll.__str__()




if __name__ == "__main__":
	queue = Queue()

	for i in range(1, 6):
		queue.enqueue(i)

	assert(queue.size() == 5)

	print 'Queue after enqueues', queue

	for i in range(1, 6):
		assert(i == queue.dequeue())
		assert(queue.size() == (5-i))


	q2 = Queue()
	q2.enqueue('+')
	assert('+' == q2.dequeue())
	q2.enqueue('a')
	q2.enqueue('b')
	assert('a' == q2.dequeue())
	assert('b' == q2.dequeue())

	print 'Queue testcases passed'
