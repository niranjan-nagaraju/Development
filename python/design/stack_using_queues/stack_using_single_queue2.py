'''
Implement stack using a (single) queue.

Approach:
  *Pop-efficient version*

  + Use a queue, Q,
    + push(x):
      Enqueue x into Q
      Dequeue (n-1) items from Q back into Q

    + pop(x):
      Dequeue from Q and return

    + Test runs
	  - push(1):
		Q: 1

	  - push(2):
		Q: 1 2
		--
		Q: 2 1

	  - push(3):
		Q: 2 1 3
		-- 
		Q: 1 3 2
		--
		Q: 3 2 1

	  - pop(): return 3
		Q: 3 2 1
		-- Q.dequeue() == 3
		Q: 2 1

	  - push(4):
		Q: 2 1 4
		--
		Q: 1 4 2
		--
		Q: 4 2 1

	  - push(5):
		Q: 4 2 1 5
		--
		Q: 2 1 5 4
		--
		Q: 1 5 4 2
		--
		Q: 5 4 2 1

	  - pop(): return 5
		Q: 5 4 2 1
		-- Q.dequeue() == 5
		Q: 4 2 1

	  - pop(): return 4
		Q: 4 2 1
		-- Q.dequeue() == 4
		Q: 2 1

	  - pop(): return 2
		Q: 2 1
		-- Q.dequeue() == 2
		Q: 1

	  - pop(): return 1
		Q: 1
		-- Q.dequeue() == 1
		Q:
'''

from data_structures.sll.queue import Queue

class Stack(object):
	def __init__(self):
		self.q = Queue()


	# peek what's at the top of the stack
	# Stack-order is preserved in Q
	# Stack's top would be at the front of Q
	def top(self):
		return self.q.front()


	def __len__(self):
		return self.q.size


	# Enqueue x into Q
	# re-enqueue (n-1) items from Q, and re-enqueue back to Q
	def push(self, x):
		# Helper function to reenqueue 'n' items from queue a back into it
		def reenqueue(a, n):
			for i in range(n):
				a.enqueue(a.dequeue())

		self.q.enqueue(x)
		# reenqueue (n-1) items from q
		reenqueue(self.q, self.q.size-1)



	# Just dequeue from Q as it preserves stack-order
	def pop(self):
		# Dequeue from the front of q
		return self.q.dequeue()



	# Q preserves stack-order, Just return Q as-is
	def __str__(self):
		return str(self.q)


	def __repr__(self):
		return "%r" %(self.q)



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
	assert("%r" %(s) == "[4]: 5 4 2 1")

	sl = [5,4,2,1]
	i = 0
	while s:
		assert(len(s) == (4-i))
		assert(s.pop() == sl[i])
		i += 1

	print 'Stack testcases passed'



