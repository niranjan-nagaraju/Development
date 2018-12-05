'''
PLACEHOLDER:
  Use two queues, track current minima, and move from one queue to another when the current minima is dequeued from either queues.
  Enqueue(): Enqueue into current queue whichever it is, update minima.
	=> In practice, Enqueue into Q1, update minima
  Dequeue(): Dequeue from current queue, if dequeued item == minima, 
     then move everything from current queue to the other, make other the current queue.
	 => In practice, Dequeue from minima, if if dequeud item == minima,, 
	    then everything from Q1 to Q2, swap Q1, Q2
  minimum(): == minima

	  + Enqueue(1)
		Q1: 1
		min: 1
		Q2:

	  + Enqueue(2)
		Q1: 1 2
		min: 1
		Q2:

	  + Enqueue(3)
		Q1: 1 2 3
		min: 1
		Q2:

	  + minimum(): 1

	  + Dequeue() -> should return 1
		Q1: 1 2 3 -> dequeue(Q1) returns 1 == min,
		min: 1
		Q2:
		=> move everything else from Q1 to Q2 while updating min 
		Q1: 
		min: 2
		*Q2: 2 3
		=> swap
		Q1: 2 3
		min: 2
		Q2: 

	  + minimum(): 2

	  + Enqueue(4)
		Q1: 2 3 4
		min: 2
		Q2: 

	  + minimum(): 2


	  + Dequeue() -> should return 2
		Q1: 2 3 4
		min: 2
		Q2:
		Q1: 2 3 4 -> dequeue() returns 2 == min => move everything else from Q1 to Q2, 
			(update min while enqueuing one by one into Q2)
		=>
		Q2: 3 4
		min: 3
		Q1: 
		=> swap Q1, Q2
		Q1: 3 4
		min: 3
		Q2:

	  + minimum(): 3


	  + Dequeue() -> should return 3
		Q1: 3 4 -> dequeue() returns 3 == min => move everything else from Q1 to Q2, 
			(update min while enqueuing one by one into Q2)
		min: 3
		Q2: 
		=>
		Q1: 
		min: 4
		Q2: 4
		=> swap Q1, Q2
		Q1: 4
		min: 4
		Q2:

	  + minimum(): 4

	  + Dequeue() -> should return 4
		Q1: 4
		min: 4
		Q2: 
		-> dequeue() returns 4 == min => move everything else from Q1 to Q2, 
		   (update min while enqueuing one by one into Q2)
		=>
		Q1:
		min: 
		Q2: 
'''

from data_structures.sll.queue import Queue
from data_structures.sll.sll import UnderFlowError


class QueueMin(object):
	def __init__(self):
		self.q1 = Queue()
		self.q2 = Queue()
		self.min = None


	# All queue items are always in Q1, Q2 is merely used to move queue elements and update minima
	# after which Q2 is renamed Q1
	def __len__(self):
		return self.q1.size


	def __repr__(self):
		return "{%r , %r}/Min: %r" %(self.q1, self.q2, self.min)


	def __str__(self):
		return str(self.q1)


	def minimum(self):
		return self.min


	# Add to Q1 and return
	def enqueue(self, x):
		self.q1.enqueue(x)

		# update min
		# if min is None => this is the first item to be enqueued
		self.min = min(x, self.min) if self.min else x


	# Dequeue() from Q2 
	# if popped item was minima, use Q2 to move all items while calculating new minima
	def dequeue(self):
		# Helper function to move all items from Queue, A to Queue, B
		# while updating new minima
		def move(a, b):
			minima = a.front()
			while a:
				x = a.dequeue()
				b.enqueue(x)
				minima = min(x, minima)
			return minima

		# SLL raises UnderFlowError if Q2 is empty
		item = self.q1.dequeue()

		# We just dequeued the last item from the queue
		# restore min to be None
		if not self.q1:
			self.min = None
		elif item == self.min:
			# Update min if item == minima
			self.min = move(self.q1, self.q2)
			# swap Q1, Q2
			self.q1, self.q2 = self.q2, self.q1

		return item


	# Return the front of Q1
	def front(self):
		return self.q1.front()


	# Return the back of Q1
	def back(self):
		return self.q1.back()



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
