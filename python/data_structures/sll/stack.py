from sll import SLL

# Stack using a SLL
class Stack:
	def __init__(self):
		self.sll = SLL()

	def size(self):
		return self.sll.size

	# Push 'value' on top of the stack 
	def push(self, value):
		self.sll.push(value)


	# Pop top of the stack and return
	def pop(self):
		return self.sll.pop()


	def __str__(self):
		return self.sll.__str__()




if __name__ == "__main__":
	stack = Stack()

	for i in range(1, 6):
		stack.push(i)

	assert(stack.size() == 5)

	print 'Stack after push operations', stack

	for i in range(1, 6):
		assert((6-i) == stack.pop())
		assert(stack.size() == (5-i))


	s2 = Stack()
	s2.push('+')
	assert('+' == s2.pop())
	assert(s2.size() == 0)
	assert(s2.sll.head == None)
	assert(s2.sll.tail == None)

	s2.push('a')
	s2.push('b')
	assert('b' == s2.pop())
	assert('a' == s2.pop())

	print 'Stack testcases passed'

