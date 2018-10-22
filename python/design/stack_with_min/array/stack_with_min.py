'''
An array-based stack which supports min() in O(1) 
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

from data_structures.array.stack.stack import Stack

class StackMin(Stack):
	def __init__(self, capacity=0):
		self.min = None
		Stack.__init__(self, capacity)


	# Override parent stack class' push() to update min
	def push(self, item):
		if (self.size == 0):
			self.min = item

		# Update current stack min
		self.min = min(item, self.min)

		# TODO: This is assuming push always succeeds
		# revise
		super(StackMin, self).push((item, self.min))


	# Override parent stack class' pop() to update min
	def pop(self):
		item, m = super(StackMin, self).pop()

		# Update min from current stack TOS
		# reset to None if we just popped the last item off the stack
		if self.size != 0:
			self.min = self.items[self.tos][1]
		else:
			self.min = None

		return item


	# O(1) Min operation
	def minimum(self):
		return self.min


	def __str__(self):
		return super(StackMin, self).__str__() + ' Min: ' + str(self.min)



# Basic UT
if __name__ == '__main__':
	s = StackMin(10)

	for i in range(5, 0, -1):
		s.push(i)
		assert(s.size == 5-i+1)
		assert(s.minimum() == i)

	for i in range(1, 6, 1):
		s.push(i)
		assert(s.size == 5+i)
		assert(s.minimum() == 1)

	print 'Stack after pushing 10 items: ', s

	for i in range(5, 0, -1):
		assert(s.minimum() == 1)
		assert(s.pop() == i)

	for i in range(1, 6):
		assert(s.minimum() == i)
		assert(s.pop() == i)

	print 'Stack with Min testcases passed!'
