from sll import SLL

# Queue using a SLL
class Queue(SLL):
	def length(self):
		return self.size


if __name__ == "__main__":
	queue = Queue()

	for i in range(1, 6):
		queue.enqueue(i)

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

	print 'Queue testcases passed'