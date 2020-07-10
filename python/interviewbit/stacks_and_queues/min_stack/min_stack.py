'''
https://www.interviewbit.com/problems/min-stack/

Min Stack

Design a stack that supports push, pop, top, and retrieving the minimum element in constant time.

	push(x) - Push element x onto stack.
	pop() - Removes the element on top of the stack.
	top() - Get the top element.
	getMin() - Retrieve the minimum element in the stack.

Note that all the operations have to be constant time operations.

Questions to ask the interviewer :
	Q: What should getMin() do on empty stack? 
	A: In this case, return -1.

	Q: What should pop do on empty stack? 
	A: In this case, nothing. 

	Q: What should top() do on empty stack?
	A: In this case, return -1
'''

'''
Solution Outline:
	1. Store current stack minimum along-side each item.
	2. When pushing item, x, onto the stack, check if the current stack minimum > x
		If so, push (x,x) onto the stack
		Otherwise push(x, current minimum)
	3. getMin(): return minimum stored at the stack top
	4. pop() and top() returns/removes the item+stack minimum stored at the stack top
	
Sample run:
	MinStack: []
	
	push(1)
	MinStack: [(1,1)]
	getMin(): 1

	push(2)
	MinStack: [(1,1), (2,1)]
	getMin(): 1

	pop()
	MinStack: [(1,1)]
	getMin(): 1

	pop()
	MinStack: []
	
	push(3)
	MinStack: [(3,3)]
	getMin(): 3

	push(2)
	MinStack: [(3,3), (2,2)]
	getMin(): 2

	push(1)
	MinStack: [(3,3), (2,2), (1,1)]
	getMin(): 1

	pop()
	MinStack: [(3,3), (2,2)]
	getMin(): 2
'''
class MinStack:
	def __init__(self):
		self.items = []

	# return 'item' at stack top
	def top(self):
		if not self.items:
			return -1
		return self.items[-1][0]

	# Flush stack top
	def pop(self):
		if not self.items:
			return
		self.items.pop()

	# Push (x, stack minimum) onto the stack
	def push(self, x):
		new_min = x
		if self.items:
			new_min = min(self.getMin(), x)

		self.items.append((x, new_min))

	# return 'minimum' at stack top
	def getMin(self):
		if not self.items:
			return -1
		return self.items[-1][1]


if __name__ == '__main__':
	ms = MinStack()
	assert ms.top() == -1
	assert ms.getMin() == -1
	ms.push(5)
	assert ms.top() == 5
	assert ms.getMin() == 5

	ms.push(6)
	assert ms.top() == 6
	assert ms.getMin() == 5

	ms.pop()
	ms.push(1)
	assert ms.top() == 1
	assert ms.getMin() == 1


