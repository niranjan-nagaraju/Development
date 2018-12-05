'''
  Based on Queue_with_min2 but uses a single queue instead of two 	
  Use a single queue, track current minima, and re-enqueue items when the current minima is dequeued.
  Enqueue(): Enqueue into queue Q, update minima.
  Dequeue(): Dequeue from queue Q, if dequeued item == minima, 
     then re-enqueue everything back into Q
  minimum(): == minima

	  + Enqueue(1)
		Q: 1
		min: 1

	  + Enqueue(2)
		Q: 1 2
		min: 1

	  + Enqueue(3)
		Q: 1 2 3
		min: 1

	  + minimum(): 1

	  + Dequeue() -> should return 1
		Q: 1 2 3 -> dequeue(Q) returns 1 == min,
		min: 1
		=> re-enqueue everything from Q back into it while updating min 
		Q: 2 3
		min: 1
		--
		Q: 3 2
		min: 2
		--
		Q: 2 3
		min: 2

	  + minimum(): 2

	  + Enqueue(4)
		Q: 2 3 4
		min: 2

	  + minimum(): 2

	  + Dequeue() -> should return 2
		Q: 2 3 4 => dequeue() returns 2
		min: 2
		=> re-enqueue everything from Q back into it while updating min 
		Q: 3 4
		min: 3

	  + minimum(): 3

	  + Dequeue() -> should return 3
		Q: 3 4 -> dequeue() returns 3 == min 
		min: 3
		=> re-enqueue everything from Q back into it while updating min 
		Q: 4
		min: 4
		=>

	  + minimum(): 4

	  + Dequeue() -> should return 4
		Q: 4
		min: 4
		-> dequeue() returns 4 == min
		=> re-enqueue everything from Q back into it while updating min 
		Q:
		min: 
'''

from data_structures.sll.queue import Queue
from data_structures.sll.sll import UnderFlowError


class QueueMin(object):
	def __init__(self):
		self.q = Queue()
		self.min = None


	# All queue items are always in Q, 
	def __len__(self):
		return self.q.size


	def __repr__(self):
		return "{%r}/Min: %r" %(self.q, self.min)


	def __str__(self):
		return str(self.q)


	def minimum(self):
		return self.min


	# Add to Q and return
	def enqueue(self, x):
		self.q.enqueue(x)

		# update min
		# if min is None => this is the first item to be enqueued
		self.min = min(x, self.min) if self.min else x


	# Dequeue() from Q 
	# if popped item was minima, Re-enqueue all items while calculating new minima
	def dequeue(self):
		# Helper function to re-enqueue all items from Queue, Q back into itself
		# while updating new minima
		def reenqueue(q):
			minima = q.front()
			for i in range(len(q)):
				x = q.dequeue()
				q.enqueue(x)
				minima = min(x, minima)
			return minima

		# SLL raises UnderFlowError if Q is empty
		item = self.q.dequeue()

		# We just dequeued the last item from the queue
		# restore min to be None
		if not self.q:
			self.min = None
		elif item == self.min:
			# Update min if item == minima
			self.min = reenqueue(self.q)

		return item


	# Return the front of Q
	def front(self):
		return self.q.front()


	# Return the back of Q
	def back(self):
		return self.q.back()



if __name__ == '__main__':
	qm = QueueMin()

	for i in range(5, 2, -1):
		qm.enqueue(i)
		assert(qm.minimum() == i)
		print "Adding %d %r\n%s" %(i, qm, qm)
		assert(qm.front() == 5)
		assert(qm.back() == i)

	assert(qm.front() == 5)
	assert(len(qm) == 3)
	assert(qm.minimum() == 3)
	assert(qm.dequeue() == 5)
	assert(qm.front() == 4)
	assert(qm.minimum() == 3)

	qm.enqueue(2)
	assert(qm.back() == 2)
	assert(qm.minimum() == 2)

	assert(qm.dequeue() == 4)
	assert(qm.minimum() == 2)
	assert(qm.dequeue() == 3)
	assert(qm.minimum() == 2)
	assert(qm.dequeue() == 2)

	assert(len(qm) == 0)
	assert(qm.minimum() == None)

	try:
		assert(qm.dequeue() == None)
	except UnderFlowError:
		print "Queue Underflow"

	print "Queue %r\n%s" %(qm, qm)

	print 'Queue with min testcases succeeded!'
