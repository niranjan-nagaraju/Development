# Exceptions for the stack class
class OverFlowError(Exception):
	''' Stack Overflow '''
	pass

class UnderFlowError(Exception):
	''' Stack Underflow '''
	pass


class Stack(object):
	def __init__(self, capacity=0):
		self.capacity = capacity # 0 capacity => no checks for limits
		self.size = 0
		self.tos = -1
		self.items = [0] * capacity 

	def push(self, item):
		if self.capacity != 0 and self.capacity == self.size:
			# overflow
			raise OverFlowError
			return

		self.size += 1
		self.tos += 1
		self.items[self.tos] = item

	def pop(self):
		if self.size == 0:
			# underflow
			raise UnderFlowError
			return None
		
		self.size -= 1
		item = self.items[self.tos]
		self.tos -= 1

		return item

	def __str__(self):
		stack_str = '[' + str(self.size) + ']: '
		for i in range(self.size):
			stack_str += str(self.items[i]) + ' '

		return stack_str


	def __repr__(self):
		return "[%d]: %r" %(self.size, self.items[:self.size])


def TC():
	stack = Stack(5)
	try:
		stack.pop() # shouldn't crash
	except UnderFlowError:
		print 'Stack Underflow - popped an empty stack!'

	for i in range(1, 7):
		try:
			print 'Pushing ', i,
			stack.push(i)
			print "Current stack: ", stack
		except OverFlowError:
			print 'Stack Overflow!, stack capacity:', stack.capacity

	print 'repr: %r' %(stack)

	for i in range(1, 7):
		try:
			print 'Popping ', stack.pop(),
			print "Current stack: ", stack
		except UnderFlowError:
			print "Stack Underflow!"

	assert(stack.tos  == -1)


if __name__ == '__main__':
	TC()
