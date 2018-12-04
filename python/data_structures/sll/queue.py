from sll import SLL
from data_structures.sll.sll import UnderFlowError

# Queue using a SLL
class Queue(SLL):
	def __init__(self):
		SLL.__init__(self)

		# Meaningful aliases for enqueue and dequeue
		# for queue operations
		self.enqueue = self.push_back
		self.dequeue = self.pop_front


	def length(self):
		return self.size


	# return the item at the front of the Queue
	def front(self):
		if not self.head:
			raise UnderFlowError
		return self.head.value


	# return the item at the end of the Queue
	def back(self):
		if not self.tail:
			raise UnderFlowError
		return self.tail.value



if __name__ == "__main__":
	queue = Queue()

	for i in range(1, 6):
		queue.enqueue(i)
		assert(queue.front() == 1)
		assert(queue.back() == i)

	assert(queue.length() == 5)

	print 'Queue after enqueues', queue

	for i in range(1, 6):
		assert(i == queue.dequeue())
		assert(queue.length() == (5-i))


	q2 = Queue()
	q2.enqueue('+')
	assert('+' == q2.dequeue())
	q2.enqueue('a')
	q2.enqueue('b')
	assert('a' == q2.dequeue())
	assert('b' == q2.dequeue())

	try:
		assert(q2.dequeue() == None)
	except:
		# Underflow error
		print 'Underflow error'

	print 'Queue testcases passed'
