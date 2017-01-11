'''
A stack which supports min() in O(1) 
in addition to the exisitng push() and pop() in O(1)
'''

from data_structures.array.stack.stack import *

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
			self.min = 0

		return item

	def min(self):
		if self.size == 0:
			return -1

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
