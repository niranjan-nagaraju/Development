'''
A Queue which supports min() in O(1) 
in addition to the exisitng enqueue() and dequeue()

min(): returns the current minimum in the stack

Approach:
 Either enqueue() or dequeue() takes a hit to support O(1) min.

 Uses two concepts
  1. Stack with minimum
     Stack with minimum() is simpler because there is a 'recency bias'
     push (data, current minima) to the stack instead of just data.
     minimum() -> stack top

	+ push(3)
	  Stack: (3,3)

	+ push(2)
	  Stack: (2,2) (3,3)
     
	+ minimum() -> returns 2

	+ push(1)
	  Stack: (1,1) (2,2) (3,3)

	+ minimum() -> returns 1

	+ pop() -> removes 1
	  Stack: (2,2) (3,3)

	+ minimum() -> returns 2

  2. Queue implemented using 2 stacks
	 Enqueue() -> always push to S1
	 Dequeue() -> always pop from S2, unless S2 is empty, in which case, push everything from S1 to S2 and then pop from S2

	+ Enqueue(1)
	  S1: 1
	  S2:

	 + Enqueue(2)
	   S1: 2 1
	   S2:

	 + Enqueue(3)
	   S1: 3 2 1
	   S2: 

	 + Enqueue(4)
	   S1: 4 3 2 1
	   S2:

	 + Dequeue() -> should return 1
	   S1: 
	   S2: 1 2 3 4    -> pop(S2) -> 2 3 4

	 + Enqueue(5)
	   S1: 5
	   S2: 2 3 4

	 + Enqueue(6)
	   S1: 6 5
	   S2: 2 3 4

	 + Dequeue()  -> returns 2
	   S1: 6 5
	   S2: 2 3 4 -> 3 4

	 + Dequeue() -> returns 3
	   S1: 6 5
	   S2: 3 4 -> 4

	 + Dequeue() -> returns 4
	   S1: 6 5
	   S2: 4 -> []

	 + Dequeue() -> returns 5
	   S1: 6 5 (push all to S2)
	   S2:
	   =>
	   S1:
	   S2: 5 6 -> pop(S2) -> 6

	 + Dequeue() -> returns 6
	   S1:
	   S2: 6 -> pop(6) -> []
	   =>
	   S1:
	   S2:


   3. Use 1. and 2. to implement Queue with minimum
	  Uses two stacks with minimum, S1, and S2
	  NOTE: S1 can be a regular stack with an additional external state maintaining S1's min at any given time.
			S1's minimum snapshots are not used since they are all popped at once into S2 where the minimums are updated 
			for the queue-order.
			However, Using StackMin for both S1 and S2 is more consistent (symmetric?), plus S1 min is maintaned internally at S1 
			without needing to update manually


	  + Enqueue(): 
		pushes items to S1 

	  + Dequeue(): 
		Pops from S2 if not empty, 
		Else pop()  all (only items, not minimum snapshots) from S1 into S2(which maintains minima) and pop from S2
		(The minimum snapshots from S1 are not needed while moving into S2 because they will not be relevant anymore)

	  + minimum(): min(s1.minimum(), S2.minimum())

	  + Enqueue(5)
	    S1: (5,5)
	    S1.min: 5
	    S2:
		S2.min:

	  + Enqueue(4)
	    S1: (4,4) (5,5)
	    S1.min: 4
	    S2:
	    S2.min: 

	  + Enqueue(3)
	    S1: (3,3) (4,4) (5,5)
	    S1.min: 3
	    S2:
		S2.min:

	  + minimum(): S1.minimum() == 3

	  + Dequeue() -> should return 5
	    S1: => push everything from S1 to S2, and flush s1_min
	    S2:
	    =>
	    S1: 
	    S1.min: 
	    S2: (5,3) (4,3) (3,3) 
		S2.min: 3
		=> pop(S2) -> (4,3) (3,3)
		S2.min: 3

	  + minimum(): S2.minimum() == 3

	  + Enqueue(2)
	    S1: (2,2)
	    S1.min: 2
	    S2: (4,3) (3,3)
		S2.min: 3

	  + minimum(): min(S1.minimum(), S2.minimum()) == min(2, 3) == 2


	  + Dequeue() -> should return 4
	    S1: (2,2)
	    S1.min: 2
	    S2: (3,3)
		S2.min: 3

	  + Dequeue() -> should return 3
	    S1: (2,2)
	    S1.min: 2
	    S2: 
		S2.min:

	  + minimum(): S1.minimum() == 2

	  + Dequeue() -> should return 2
	    S1: (push all to S2)
	    =>
	    S1:
	    S1.min:
	    S2: (2,2)
		S2.min: 2
		=> pop() -> 2
		S2:
		S2.min:
'''

from design.stack_with_min.sll.stack_with_min import StackMin as StackWithMin
from data_structures.sll.sll import UnderFlowError


class QueueMin(object):
	def __init__(self):
		self.s1 = StackWithMin()
		self.s2 = StackWithMin()
		self.min = None


	# There can be newly pushed elements onto S1
	# while S2 has earlier elements in queue-order
	# Total items in queue would be the sum total in both the stacks
	def __len__(self):
		return self.s1.size + self.s2.size


	def __repr__(self):
		return "{%r , %r}/Min: %r" %(self.s1, self.s2, self.min)


	# Queue order is S1: bottom->top, S2: top->bottom
	# Skip 'min' portion of the stack items when constructing str(q)
	def __str__(self):
		s1_contents = []
		for x in self.s1:
			s1_contents.insert(0, str(x[0]))

		s2_contents = []
		for x in self.s2:
			s2_contents.append(str(x[0]))

		return '[%d]: ' %(self.__len__()) + ' '.join(s2_contents) + ' ' +  ' '.join(s1_contents)




	def minimum(self):
		return self.min


	# Add to S1 and return
	def enqueue(self, x):
		self.s1.push(x)

		# update min
		# we are pushing into S1, 
		# Current min is min(S1.min, S2.min)
		# New min would be updated to current item being pushed
		# if x < current min
		if not self.min: # min is None => this is the first item to be enqueued
			self.min = x
		else: # 'min' is not None, so compare it against x  
			self.min = min(x, self.min)


	# pop() from S2 if its not empty,
	# else, move items from S1 into S2, and then pop() from S2
	def dequeue(self):
		# Helper function to move all items from stack with min, a to stack with min, b
		def move(a, b):
			while a:
				b.push(a.pop())

		# s2 is empty, move everything from s1 to s2 so everything in s1 now
		# is stored into s2 in queue-order, along with respective min snapshots
		if not self.s2:
			move(self.s1, self.s2)

		# S2 already has elements in queue-order 
		# or everything in S1 was just moved into S2
		# Either ways, Item to dequeue will be in S2 top
		item = self.s2.pop()

		# Update min
		# New min would be 
		# min(S1.min, S2.min) if S1 and S2 are not both empty *OR*
		# S2.min: if S1.min is empty *OR*
		# S1.min: if S2 became empty after this dequeue()
		if not self.s1:
			self.min = self.s2.min
		elif not self.s2: # we just removed the last item from S2
			self.min = self.s1.min
		else:
			self.min = min(self.s1.min, self.s2.min) 

		return item


	#NOTE: front() cannot be implemented efficiently
	# if S2 has elements in them, front of the queue would just be S2.top()
	# as S2 order is the queue-order
	# otherwise, we have items in S1 which are in the reverse-queue-order,
	# The bottom of the stack would be the front in this case
	def front(self):
		if self.s2:
			return self.s2.top()
		else:
			for x in self.s1:
				pass
			return x[0]


	#NOTE: back() cannot be implemented efficiently
	# If S1 is empty, tail(S2) (which is in queue order) is the back of the queue
	# If S2 has elements in them, 
	# back of the queue is in S1.top() as that would be where the last item was enqueued
	def back(self):
		if not self.s1:
			for x in self.s2:
				pass
			return x[0]
		else:
			return self.s1.top()




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

