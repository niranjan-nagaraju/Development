'''
Implement a queue using stacks

Approach:
	An enqueue-efficient implementation using two stacks
	Enqueue is O(1), dequeue is O(n)

	Enqueue(x): 
		Push to S1
	Dequeue():
		if S2 has elements (they are already in queue-order)
		   return S2.pop()
		Otherwise (S2 is empty)
			Push all items from S1 to S2, S2 now has queue order
			return S2.pop()
		

	Test runs:
	Enqueue(1):
		S1: 1
		S2:
	 
	Enqueue(2):
		S1: 2 1
		S2: 

	Enqueue(3):
		S1: 3 2 1
		S2: 

	Dequeue(): 1
	    S1: 3 2 1
	    S2: 
		<--
		S1:
		S2: 1 2 3
		<-- x = S2.pop() == 1
		S1: 
		S2: 2 3

	Enqueue(4)
		S1: 4
		S2: 2 3

	Enqueue(5)
		S1: 5 4
		S2: 2 3

	Dequeue() : 2
		S1: 5 4
		S2: 2 3
		<-- S2.pop() == 2
		S1: 5 4
		S2: 3

	Dequeue(): 3
		S1: 5 4
		S2: 3
		<-- S2.pop() == 3
		S1: 5 4
		S2: 

	Dequeue(): 4
		S1: 5 4
		S2: 
		<-- 
		S1: 
		S2: 4 5

	Dequeue(): 5
'''	

from data_structures.sll.stack import Stack


class Queue(object):
	def __init__(self):
		self.s1 = Stack()
		self.s2 = Stack()

	# There can be newly pushed elements onto S1
	# while S2 has earlier elements in queue-order
	# Total items in queue would be the sum total in both the stacks
	def __len__(self):
		return self.s1.size + self.s2.size


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
			return x


	#NOTE: back() cannot be implemented efficiently
	# If S1 is empty, tail(S2) (which is in queue order) is the back of the queue
	# If S2 has elements in them, 
	# back of the queue is in S1.top() as that would be where the last item was enqueued
	def back(self):
		if not self.s1:
			for x in self.s2:
				pass
			return x
		else:
			return self.s1.top()


	def length(self):
		return self.s1.size + self.s2.size


	# Enqueue is efficient, Just push to S1 and return
	def enqueue(self, x):
		self.s1.push(x)



	# if S2 has items in it, we have atleast one item in actual queue-order, so we can just return S2.pop()
	# Otherwise, move everything from S1 into S2, essentially ordering all current items in queue-order, and then return S2.pop()
	def dequeue(self):
		# Helper function to move all items from stack a to stack b
		def move(a, b):
			while a:
				b.push(a.pop())

		# s2 is empty, move everything from s1 to s2 so everything in s1 now
		# is stored into s2 in queue-order
		if not self.s2:
			move(self.s1, self.s2)

		# S2 already has elements in queue-order 
		# or everything in S1 was just moved into S2
		# Either ways, Item to dequeue will be in S2 top
		return self.s2.pop()



	# NOTE: Queue elements is S2: top->bottom followed by S1: bottom->top
	def __str__(self):
		s1_contents = []
		for x in self.s1:
			s1_contents.insert(0, str(x))

		s2_contents = []
		for x in self.s2:
			s2_contents.append(str(x))

		return '[%d]: ' %(self.__len__()) + ' '.join(s2_contents) + ' ' +  ' '.join(s1_contents)


	def __repr__(self):
		return "{%r, %r}" %(self.s1, self.s2)



# Basic testcases
if __name__ == "__main__":
	queue = Queue()

	for i in range(1, 4):
		queue.enqueue(i)
		#print 'Queue insert %d  -- repr %r' %(i, queue)
		assert(queue.front() == 1)
		assert(queue.back() == i)

	assert(queue.length() == 3)
	assert(queue.dequeue() == 1)
	#print 'Queue dequeuing %d -- repr %r' %(1, queue)

	queue.enqueue(4)
	#print 'Queue insert %d -- repr %r' %(4, queue)
	queue.enqueue(5)
	#print 'Queue insert %d -- repr %r' %(5, queue)

	print 'Queue after enqueues & dequeues', queue

	for i in range(2, 6):
		assert(i == queue.dequeue())
		#print 'Queue dequeuing %d -- repr %r' %(i, queue)
		assert(len(queue) == (5-i))


	q2 = Queue()
	q2.enqueue('+')
	q2.enqueue('a')
	assert('+' == q2.dequeue())
	q2.enqueue('b')
	#print 'Queue2 repr %r' %(q2)
	assert('a' == q2.dequeue())
	assert('b' == q2.dequeue())


	q3 = Queue()
	q3.enqueue(('a', 1))
	q3.enqueue(('b', 2))
	assert(q3.dequeue() == ('a',1))
	q3.enqueue(('c', 3))
	#print 'Q3 repr: %r' %(q3)

	print 'Queue testcases passed'



