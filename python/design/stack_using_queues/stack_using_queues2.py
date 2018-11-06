'''
Implement a stack using queues

Approach:
  *Pop-efficient version*

  + Use two queues, Q1 and Q2,
	+ push(x):
      Enqueue x into Q2
      Dequeue all items from Q1 into Q2
      Swap Q1, Q2

	+ pop(x):
      Dequeue from Q1 and return

    + Test runs
	  - push(1)
		Q1: 
		Q2: 1
		-- swap
		Q1: 1
		Q2: 

	  - push(2):
		Q1: 1
		Q2: 2
		--
		Q1:  
		Q2: 2 1
		-- swap
		Q1: 2 1
		Q2:

	  - push(3):
		Q1: 2 1
		Q2: 3
		--
		Q1: 
		Q2: 3 2 1
		-- swap
		Q1: 3 2 1
		Q2:

	  - pop(): return 3
		Q1: 3 2 1
		Q2:
		-- return Q1.dequeue() == 3
		Q1: 2 1
		Q2:

	  - push(4):
		Q1: 2 1
		Q2: 4
		--
		Q1: 
		Q2: 4 2 1
		-- swap:
		Q1: 4 2 1
		Q2:

	  - pop(): return 4
		Q1: 4 2 1
		-- return 4
		Q1: 2 1

	  - pop(): return 2
		Q1: 2 1
		-- return 2
		Q1: 1

	  - pop(): return 1
		Q1: 1
		-- return 1
		Q1:
		Q2:
'''	

from data_structures.sll.queue import Queue

class Stack(object):
	def __init__(self):
		self.q1 = Queue()
		self.q2 = Queue()


	# peek what's at the top of the stack
	# Stack-order is preserved in Q1
	# Stack's top would be at the front of Q1
	def top(self):
		return self.q1.front()


	def __len__(self):
		return self.q1.size


	# Add x to Q2
	# Move everything from Q1 to Q2, so Q2 preserves stack-order
	# swap Q1, Q2
	def push(self, x):
		# Helper function to move all items from queue a to queue b
		def move(a, b):
			while a:
				b.enqueue(a.dequeue())

		self.q2.enqueue(x)
		# Move items from q1 to q2
		move(self.q1, self.q2)
		# Swap Q1, Q2
		self.q1, self.q2 = self.q2, self.q1


	# Q1 has items in stack-order
	# Just dequeue from Q1 and return
	def pop(self):
		return self.q1.dequeue()


	def __str__(self):
		return str(self.q1)


	def __repr__(self):
		return "{%r , %r}" %(self.q1, self.q2)



# Basic testcases
if __name__ == "__main__":
	s = Stack()

	for i in range(1, 4):
		s.push(i)
		assert(s.top() == i)

	assert(len(s) == 3)
	assert(s.pop() == 3)

	s.push(4)
	s.push(5)

	assert(str(s) == "[4]: 5 4 2 1")
	assert("%r" %(s) == "{[4]: 5 4 2 1 , [0]:}")	

	sl = [5,4,2,1]
	i = 0
	while s:
		assert(len(s) == (4-i))
		assert(s.pop() == sl[i])
		i += 1

	print 'Stack testcases passed'


