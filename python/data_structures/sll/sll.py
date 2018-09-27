from node import Node

# Exceptions for the SLL class
class OverFlowError(Exception):
	''' List Overflow '''
	pass

class UnderFlowError(Exception):
	''' List Underflow '''
	pass



class SLL:
	def __init__(self):
		self.head = None
		self.tail = None
		self.size = 0

		# function aliases for better readability
		# enqueue() and dequeue():
		#  Can be push_front/pop_back too, but usually u join at the back of q queue,
		#  and get processed at the front
		self.enqueue = self.push_back
		self.dequeue = self.pop_front
		self.push = self.push_back
		self.pop = self.pop_back


	# Insert at front
	def push_front(self, value):
		node = Node(value)
		self.size += 1

		node.next = self.head
		self.head = node

		if (not self.tail):
			self.tail = node


	# Insert to the rear
	def push_back(self, value):
		node = Node(value)

		self.size += 1

		# No elements in the list
		if not self.tail:
			self.tail = node
			self.head = node
			return

		self.tail.next = node
		self.tail = node


	# remove first element from the SLL
	def pop_front(self):
		if not self.head:
			raise UnderFlowError
			return None

		self.size -= 1
		value = self.head.value
		self.head = self.head.next

		# We just popped the last element in the SLL,
		# Update tail
		if self.size == 0:
			self.tail = None

		return value


	# remove last element from the SLL	
	def pop_back(self):
		if not self.head:
			raise UnderFlowError
			return None

		self.size -= 1
		value = self.tail.value

		# There was only 1 element in the SLL
		if self.size == 0:
			self.head = self.tail = None
			return value

		trav = self.head
		# traverse until we reach the penultimate node in the SLL
		while trav.next != self.tail:
			trav = trav.next

		# make penultimate node the new tail, and cut old tail from its link
		trav.next = None
		self.tail = trav

		return value





	def __str__(self):
		sll_str = '[' + str(self.size) + ']: '
		trav = self.head
		while (trav):
			sll_str += str(trav) + ' '
			trav = trav.next

		return sll_str


	# Reverse an SLL(iterative version)
	def reverse(self):
		if not self.head:
			return None

		first = self.head
		second = first.next
		while second:
			third = second.next

			# Make second->first link
			second.next = first

			first = second
			second = third

		# Mark erstwhile  head.next to None, so the SLL chain ends
		self.head.next = None 

		# When all's done, 'first' is pointing to the 'tail'
		# of the SLL
		# Since we have now reversed, point 'head' to 'first',
		# but before that, Update 'tail' to erstwhile 'head'
		self.tail = self.head
		self.head = first



def test_reverse():
	sll = SLL()
	sll.push_back(1)
	sll.push_back(2)
	sll.push_back(3)
	sll.push_back(4)

	print "Before reversing: sll", sll
	sll.reverse()
	print "After reversing: sll", sll


def TC1():
	sll = SLL()
	sll.push_back(1)
	assert(sll.size == 1)
	assert(sll.head.value == 1) 
	assert(sll.tail.value == 1) 

	sll.push_front(0)
	assert(sll.size == 2)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 1) 


	sll.push_back(2)
	assert(sll.size == 3)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 2) 

	sll.enqueue(3)
	assert(sll.size == 4)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 3) 

	sll.push(4)
	assert(sll.size == 5)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 4) 

	assert(4 == sll.pop_back())
	assert(sll.size == 4)
	assert(sll.head.value == 0) 
	assert(sll.tail.value == 3) 

	assert(0 == sll.pop_front())
	assert(sll.size == 3)
	assert(sll.head.value == 1) 
	assert(sll.tail.value == 3) 

	assert(1 == sll.dequeue())
	assert(sll.size == 2)
	assert(sll.head.value == 2) 
	assert(sll.tail.value == 3) 

	assert(3 == sll.pop())
	assert(sll.size == 1)
	assert(sll.head.value == 2) 
	assert(sll.tail.value == 2) 


	assert(2 == sll.pop_back())
	assert(sll.size == 0)
	assert(sll.head == None) 
	assert(sll.tail == None) 

	print 'TC1 passed'


if __name__ == "__main__":
	TC1()
	test_reverse()

