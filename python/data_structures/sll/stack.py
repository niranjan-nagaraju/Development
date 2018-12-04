from sll import SLL, UnderFlowError

# Stack using a SLL
class Stack(SLL):
	def __init__(self):
		SLL.__init__(self)

		# Meaningful aliases for push and pop
		# for stack operations
		self.push = self.push_front
		self.pop = self.pop_front


	def length(self):
		return self.size


	def top(self):
		if not self.head:
			raise UnderFlowError
		return self.head.value



if __name__ == "__main__":
	stack = Stack()

	for i in range(1, 6):
		stack.push(i)
		assert(stack.top() == i)

	assert(stack.length() == 5)

	print 'Stack after push operations', stack

	for i in range(1, 6):
		assert((6-i) == stack.pop())
		assert(stack.length() == (5-i))


	s2 = Stack()
	s2.push('+')
	assert('+' == s2.pop())
	assert(s2.length() == 0)
	assert(s2.head == None)
	assert(s2.tail == None)

	s2.push('a')
	s2.push('b')
	assert('b' == s2.pop())
	assert('a' == s2.pop())

	try:
		assert(s2.pop() == None)
	except UnderFlowError:
		print 'Underflow'


	print 'Stack testcases passed'

