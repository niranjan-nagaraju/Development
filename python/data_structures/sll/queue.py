from sll import *

# Queue using a SLL
class Queue:
	def __init__(self):
		self.sll = SLL()

	def size(self):
		return self.sll.size

	# enqueue and return DLL node (will be the tail)
	def enqueue(self, value):
		self.sll.enqueue(value)


	# dequeue and return the DLL node (will be the previous head)
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

	print 'Queue testcases passed'

