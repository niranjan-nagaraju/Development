'''
An SLL-based stack which supports min() in O(1) 
in addition to the exisitng push() and pop() in O(1)

min(): returns the current minimum in the stack

e.g.
push(5)
push(4)
push(3)
min() -> returns 3
pop() -> returns 3
min() -> returns 4 as 4 is the new minimum
push(1)
min() -> returns 1

Approach:
	Over a sequence of push() and pop(), min() keeps changing => we need to keep the 'current min' as part of the stack entries.
	Further, since a stack is a LIFO, and pop() returns newer entries, storing current min along with each push() would keep track of
	relative min() as the stack grew.

	push(5):
	   stack: (5,5)
	push(4):
		stack: (5,5), (4,4)
	push(3):
		stack: (5,5), (4,4), (3,3)
	min() -> return min from tos -> 3
	pop() -> return (3,3)
		stack: (5,5), (4,4) <- min() is automatically adjusted
	push(1)
		stack: (5,5), (4,4) (1,1)
	min() -> return min from tos -> 1
'''

from data_structures.sll.stack import Stack

class StackMin(Stack):
	def __init__(self):
		Stack.__init__(self)
		self.min = None


	# Override stack top to return just the item, 
	# not the (item, minimum) pair stored underneath
	def top(self):
		return Stack.top(self)[0]


	# Override parent stack class' push_front() to update min
	# NOTE: This should mean the push() alias is updated to the overridden version
	def push_front(self, item):
		if (self.size == 0):
			self.min = item

		# Update current stack min
		self.min = min(item, self.min)
		Stack.push_front(self, (item, self.min))


	# Override parent stack class' pop_front() to update min
	# NOTE: This should mean the pop_front() alias is updated to the overridden version
	def pop_front(self):
		item, m = Stack.pop_front(self)
		if self.size != 0:
			self.min = Stack.top(self)[1]
		else:
			self.min = None
		return item


	# O(1) Min operation
	def minimum(self):
		return self.min


	def __str__(self):
		sstr = "[%d]: " %(self.size)
		for x in self:
			sstr += "%s " %(x[0])

		sstr += "/Min: %s" %(self.min)
		return sstr


	def __repr__(self):
		return Stack.__str__(self) + '/Min: ' + str(self.min)



if __name__ == '__main__':
	s = StackMin()

	for i in range(5, 0, -1):
		s.push(i)
		assert(s.top() == i)
		assert(s.size == 5-i+1)
		assert(s.minimum() == i)

	assert(s.pop() == 1)
	assert(s.minimum() == 2)

	for i in range(1, 6, 1):
		s.push(i)
		assert(s.size == 4+i)
		assert(s.minimum() == 1)

	print 'Current Stack: ', s
	print 'Stack repr: %r' %(s)

	for i in range(5, 1, -1):
		assert(s.minimum() == 1)
		assert(s.pop() == i)

	for i in range(1, 6):
		assert(s.minimum() == i)
		assert(s.pop() == i)

	print 'Stack with Min testcases passed!'

