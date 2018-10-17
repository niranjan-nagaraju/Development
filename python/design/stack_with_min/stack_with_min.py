'''
A stack which supports min() in O(1) 
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

	def push(self, item):
		if (self.size == 0):
			self.min = item

		self.min = min(item, self.min)

		# TODO: This is assuming push always succeeds
		# revise
		super(StackMin, self).push((item, self.min))

	def pop(self):
		item, m = super(StackMin, self).pop()

		if self.size != 0:
			self.min = self.items[self.tos][1]
		else:
			self.min = None

		return item

	def min(self):
		return self.min

	def __str__(self):
		return super(StackMin, self).__str__() + ' Min: ' + str(self.min)


if __name__ == '__main__':
	stack = StackMin(10)

	for i in range(5, 0, -1):
		print 'Pushing', i
		stack.push(i)
		print stack

	for i in range(1, 6, 1):
		print 'Pushing', i
		stack.push(i)
		print stack

	for i in range(10):
		print 'Popped', stack.pop()
		print stack
