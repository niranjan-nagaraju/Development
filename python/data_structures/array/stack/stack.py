class Stack:
	def __init__(self, capacity=0):
		self.capacity = capacity # 0 capacity => no checks for limits
		self.size = 0
		self.tos = -1
		self.items = [0] * capacity # TODO: 0 capacity

	def push(self, item):
		if self.capacity != 0 and self.capacity == self.size:
			# overflow
			return

		self.size += 1
		self.tos += 1
		self.items[self.tos] = item

	def pop(self):
		if self.size == 0:
			# underflow
			return
		
		self.size -= 1
		item = self.items[self.tos]
		self.tos -= 1

		return item

	def __str__(self):
		stack_str = '[' + str(self.size) + ']: '
		for i in range(self.size):
			stack_str += str(self.items[i]) + ' '

		return stack_str


if __name__ == '__main__':
	stack = Stack(10)
	stack.pop() # shouldn't crash
	for i in range(1, 6):
		print 'Pushing ', i
		stack.push(i)
		print stack

	for i in range(1, 7):
		print 'Popping ', stack.pop()
		print stack
