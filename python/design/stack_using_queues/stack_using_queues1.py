'''
Implement a stack using queues

Approach:
  *Push-efficient version*

  + Use two queues, Q1 and Q2,
    + push(x):
      enqueue to Q1 and return

    + pop():
      dequeue (n-1) items from Q1 one at a time, into Q2
      dequeue remaining item from Q1 and return.
      swap Q1, Q2

    + Test runs:
	  - push(1):
		Q1: 1
		Q2:

	  - push(2)
		Q1: 1 2
		Q2:

	  - push(3)
		Q1: 1 2 3
		Q2:

	  - pop(): return 3
		Q1: 2 3
		Q2: 1
		--
		Q1: 3
		Q2: 1 2
		--
		Q1.dequeue() -- 3
		Q2: 1 2
		-- swap Q1, Q2
		Q1: 1 2
		Q2:

	  - push(4)
		Q1: 1 2 4
		Q2:

	  - pop(): return 4
		Q1: 4 -- return 4
		Q2: 1 2
		-- 
		Q1:
		Q2: 1 2
		-- swap Q1, Q2
		Q1: 1 2
		Q2:

	  - pop(): return 2
		Q1: 2 -- return 2
		Q2: 1
		-- 
		Q1:
		Q2: 1
		-- swap Q1, Q2
		Q1: 1
		Q2:

	  - pop(): return 1
		Q1: 1 -- return 1
		Q2: 
		--
		Q1:
		Q2:
'''	

from data_structures.sll.queue import Queue

class Stack(object):
	def __init__(self):
		self.q1 = Queue()
		self.q2 = Queue()


	# peek what's at the top of the stack
	# New items are always added to Q1's end
	# Stack's top would be at the back of Q1
	def top(self):
		return self.q1.back()


	def __len__(self):
		return self.q1.size


	# Just add to Q1 and return
	def push(self, x):
		self.q1.enqueue(x)


	# Move (n-1) items from q1 to q2
	# dequeue remaining item from q1 to return
	# swap q1, q2
	def pop(self):
		# Helper function to move 'n' items from queue a to queue b
		def move(a, b, n):
			for i in range(n):
				b.enqueue(a.dequeue())

		# Move (n-1) items from q1 to q2
		move(self.q1, self.q2, self.q1.size-1)

		# Swap Q1, Q2
		self.q1, self.q2 = self.q2, self.q1

		# Dequeue and return the last item from what was q1 earlier, and now q2
		return self.q2.dequeue()



	def __str__(self):
		slist = []
		for x in self.q1:
			slist.insert(0, str(x))

		return '[%d]: ' %(self.__len__()) + ' '.join(slist) 


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
	assert("%r" %(s) == "{[4]: 1 2 4 5 , [0]:}")	

	sl = [5,4,2,1]
	i = 0
	while s:
		assert(len(s) == (4-i))
		assert(s.pop() == sl[i])
		i += 1


	print 'Stack testcases passed'



