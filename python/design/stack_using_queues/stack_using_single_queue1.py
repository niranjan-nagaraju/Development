'''
Implement stack using a (single) queue.

Approach:
  *Push-efficient version*

  + Use a queue, Q,
    + push(x):
      enqueue to Q and return

    + pop():
      dequeue (n-1) items from Q one at a time, back into Q
      dequeue item from front of the Q and return.

    + Test runs:
	  - push(1):
		Q: 1

	  - push(2):
		Q: 1 2

	  - push(3):
		Q: 1 2 3

	  - pop(): return 3
		Q: 1 2 3
		--
		Q: 2 3 1
		--
		Q: 3 1 2
		-- Q.dequeue() == 3
		Q: 1 2

	  - push(4):
		Q: 1 2 4

	  - pop(): return 4
		Q: 1 2 4
		--
		Q: 2 4 1
		--
		Q: 4 1 2
		-- Q.dequeue() == 4
		Q: 1 2

	  - pop(): return 2
		Q: 1 2
		--
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
	# New items are always added to Q's end
	# Stack's top would be at the back of Q
	def top(self):
		return self.q.back()


	def __len__(self):
		return self.q.size


	# Just add to Q and return
	def push(self, x):
		self.q.enqueue(x)


	# re-enqueue (n-1) items from Q, and re-enqueue back to Q
	def pop(self):
		# Helper function to reenqueue 'n' items from queue a back into it
		def reenqueue(a, n):
			for i in range(n):
				a.enqueue(a.dequeue())

		# reenqueue (n-1) items from q
		reenqueue(self.q, self.q.size-1)
		# Dequeue from the front of q
		return self.q.dequeue()



	# Actual stack order would be the q in reverse order
	def __str__(self):
		slist = []
		for x in self.q:
			slist.insert(0, str(x))

		return '[%d]: ' %(self.__len__())  +  ' '.join(slist)


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
	assert("%r" %(s) == "[4]: 1 2 4 5")

	sl = [5,4,2,1]
	i = 0
	while s:
		assert(len(s) == (4-i))
		assert(s.pop() == sl[i])
		i += 1

	print 'Stack testcases passed'



