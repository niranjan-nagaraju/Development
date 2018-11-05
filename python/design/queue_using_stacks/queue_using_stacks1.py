'''
Implement a queue using stacks

Approach:
	An Dequeue-efficient implementation using two stacks
	enqueue is O(n), dequeue is O(1)

	Enqueue(x): 
		Use S2 to reverse order in S1 -- push all items from S1 to S2
		Add x to S1
		Push all items from S2 into S1
	Dequeue():
		x = S1.pop() 

	Test runs:
		Enqueue(1):
			S1: 1
			S2:
		 
		Enqueue(2):
			S1: 1
			S2: 
			<-- S1 -> S2
			S1: 
			S2: 1
			<-- 
			S1: 2
			S2: 1
			<--
			S1: 1 2
			S2:

		Enqueue(3):
			S1: 1 2
			S2: 
			<-- S1 -> S2
			S1: 
			S2: 2 1
			<--
			S1: 3
			S2: 2 1
			<--
			S1: 2 3
			S2: 1
			<--
			S1: 1 2 3
			S2:

		Dequeue(): 1
			S1: 1 2 3
			S2: 
			<-- x = S1.pop() == 1
			S1: 2 3
			S2:

		Enqueue(4)
			S1: 2 3
			S2:
			<--
			S1: 4
			S2: 3 2
			<--
			S1: 2 3 4
			S2:

		Enqueue(5)
			S1: 2 3 4
			S2:
			<--
			S1: 5
			S2: 4 3 2
			<--
			S1: 2 3 4 5
			S2:

		Dequeue(): 2
		Dequeue(): 3
		Dequeue(): 4
		Dequeue(): 5
'''	

from data_structures.sll.stack import Stack


class Queue(object):
	def __init__(self):
		self.s1 = Stack()
		self.s2 = Stack()


	def front(self):
		return self.s1.top()


	#NOTE: back() cannot be implemented efficiently
	def back(self):
		for x in self.s1:
			pass

		return x


	def __len__(self):
		return self.s1.size


	def length(self):
		return self.s1.length()


	def enqueue(self, x):
		# Helper function to move all items from stack a to stack b
		def move(a, b):
			while a:
				b.push(a.pop())

		# Use S2 to reverse S1's order
		move(self.s1, self.s2)

		self.s1.push(x)

		# Move back S2 to S1, reversing it again, restoring S1's queue-order
		move(self.s2, self.s1)



	def dequeue(self):
		return self.s1.pop()



	def __str__(self):
		return self.s1.__str__()


	def __repr__(self):
		return "{%r , %r}" %(self.s1, self.s2)



# Basic testcases
if __name__ == "__main__":
	queue = Queue()

	for i in range(1, 4):
		queue.enqueue(i)
		assert(queue.front() == 1)
		assert(queue.back() == i)

	assert(queue.length() == 3)
	assert(queue.dequeue() == 1)

	queue.enqueue(4)
	queue.enqueue(5)

	assert(str(queue) == "[4]: 2 3 4 5")
	assert("%r" %(queue) == "{[4]: 2 3 4 5 , [0]:}")	

	for i in range(2, 6):
		assert(i == queue.dequeue())
		assert(len(queue)== (5-i))


	q2 = Queue()
	q2.enqueue('+')
	q2.enqueue('a')
	assert('+' == q2.dequeue())
	q2.enqueue('b')
	assert('a' == q2.dequeue())
	assert('b' == q2.dequeue())

	print 'Queue testcases passed'



