'''
  Use two queues, 1 regular queue Q1 and another deque DQ2 (which will be kept sorted but will discard all elements greater than a much smaller element that comes later in time)
  WHY?
    e.g. 3 2 1, 1 is the last minima noticed, and will be the last one to be dequeued (so 3 and 2 will never be relevant as minima as soon as 1 is brought in.

  Enqueue(x):
    add to Q1,
    update minima = x if x < minima
    In DQ2, remove everything from the right if > x, and add x

  Dequeue():
      item = Q1.dequeue()
    //Item is the same as DQ2 minima, remove it from DQ2 as well
    if item == DQ2.head():
      DQ2.dequeue()

      // minima has been removed, so update  minima to current DQ2 head.
      // this is likely the same as item == DQ2.head()
      minima = DQ2.head()


   Minimum():
     == minima


  + Enqueue(4)
    Q1: 4
    min: 4
    DQ2: 4 

  + Enqueue(1)
    Q1: 4 1
    min: 1
    DQ2: (remove all > 1) and add 1
    =>
    DQ2: 1

  + Enqueue(5)
    Q1: 4 1 5
    min: 1
    DQ2: (remove all > 5) and add 5
    =>
    DQ2: 1 5


  + Enqueue(3)
    Q1: 4 1 5 3
    min: 1
    DQ2: (remove all > 3), add 3
    =>
    DQ2: 1 3

  + Minimum(): 1

 
  + Enqueue(2)
    Q1: 4 1 5 3 2
    min: 1
    DQ2: (remove all > 2), add 2
    =>
    DQ2: 1 2


  + Dequeue(): returns 4
    Q1.dequeue() == 4
    =>
    Q1: 1 5 3 2
    min: 1
    DQ2: 1 2

  + Dequeue(): returns 1
    Q1.dequeue() == 1
    =>
    Q1: 5 3 2
    DQ2: 1 2
    -> item == DQ2.head() == 1, remove 1 from DQ2
    DQ2: 2
    min: 2
    Q1: 5 3 2

  + Dequeue(): returns 5
    data: Q1.dequeue() == 5
    =>
    Q1: 3 2
    min: 2
    DQ2: 2

  + Dequeue(): returns 3
    data: Q1.dequeue() == 3
    =>
    Q1: 2
    min: 2
    DQ2: 2

  + Enqueue(6):
    Q1: 2 6
    min: 2
    DQ2: (remove all > 6) and add 6
    =>
    DQ2: 2 6

  + Dequeue():
    Q1.dequeue() == 2
    item == DQ2.head() == min
    DQ2.dequeue()
    =>
    Q1: 6
    min: 6
    DQ2: 6

  + Dequeue():
    Q1.dequeue() == 6
    item == DQ2.head() == min
    DQ2.dequeue()
    =>
    Q1: 
    min: 
    DQ2: 
'''


from data_structures.sll.queue import Queue
from data_structures.dll.deque import Deque
from data_structures.sll.sll import UnderFlowError


class QueueMin(object):
	def __init__(self):
		self.q1 = Queue()
		self.dq2 = Deque()
		self.min = None


	# All queue items are always in Q1, 
	# DQ2 is merely used to store relative order of minimas
	def __len__(self):
		return self.q1.size


	def __repr__(self):
		return "{%r , %r}/Min: %r" %(self.q1, self.dq2, self.min)


	def __str__(self):
		return str(self.q1)


	def minimum(self):
		return self.min


	# Add item to Q1
	def enqueue(self, x):
		self.q1.enqueue(x)

		# update min
		# if min is None => this is the first item to be enqueued
		self.min = min(x, self.min) if self.min else x

		# Update DQ2 to maintain the order of minimas, 
		# so when a minima is removed, the next minima can be immediately retrieved
		# remove everything from the back of DQ2 as long as it is > item x
		while self.dq2 and self.dq2.back() > x:
			self.dq2.pop_back()

		# Add item x to DQ2
		self.dq2.push_back(x)



	# Dequeue() from Q1
	# DQ2 stores relative order of minimas for the current queue items
	# if popped item was minima, update minima to DQ2 new head()
	def dequeue(self):
		# SLL raises UnderFlowError if Q2 is empty
		item = self.q1.dequeue()

		# item removed was the current minima, remove it from DQ2 as well
		# item would be found in the head of DQ2
		if item == self.min:
			self.dq2.pop_front()
			# Also, update new min which would be the new head of DQ2 after removing the current minima
			# NOTE: self.min should be None, if dq2 is empty - ie, we just removed the last item in the Queue
			self.min = self.dq2.front() if self.dq2 else None

		return item


	# Return the front of Q1
	def front(self):
		return self.q1.front()


	# Return the back of Q1
	def back(self):
		return self.q1.back()



def TC1():
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



if __name__ == '__main__':
	TC1()
