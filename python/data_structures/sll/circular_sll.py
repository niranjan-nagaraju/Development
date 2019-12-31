from sll import SLL, UnderFlowError
from node import Node

class CircularSLL(SLL):
	def __str__(self):
		sll_str = '[' + str(self.size) + ']: '

		if self.size == 0:
			return sll_str

		trav = self.head
		while (trav.next != self.head):
			sll_str += str(trav) + ' '
			trav = trav.next

		sll_str += str(trav)
		return sll_str.strip()


	# Create a circular SLL from a list
	@staticmethod
	def fromList(lst):
		s = CircularSLL()
		for x in lst:
			s.push_back(x)

		return s


	# Return a List constructed from circular SLL items
	def toList(self):
		l = []
		if self.size == 0:
			return l

		trav = self.head
		while (trav.next != self.head):
			l.append(trav.value)
			trav = trav.next

		l.append(trav.value)
		return l


	# Insert at front of the circular SLL
	def push_front(self, value):
		node = Node(value)
		self.size += 1

		node.next = self.head
		self.head = node

		if (not self.tail):
			self.tail = node

		self.tail.next = self.head


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
		self.tail.next = self.head


	# remove first element from the SLL
	def pop_front(self):
		if not self.head:
			raise UnderFlowError
			return None

		self.size -= 1
		value = self.head.value

		# We just popped the last element in the SLL,
		# Update tail, head
		if self.size == 0:
			self.tail = None
			self.head = None
		else:
			self.head = self.head.next
			self.tail.next = self.head

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
		self.tail.next = self.head

		return value


if __name__ == '__main__':
	csll = CircularSLL()
	csll.push_front(1)
	assert csll.tail.next == csll.head
	csll.push_front(2)
	assert csll.tail.next == csll.head

	csll.push_back(3)
	assert csll.tail.next == csll.head

	csll.push_back(4)
	assert csll.tail.next == csll.head
	assert csll.tail.next == csll.head

	assert csll.toList() == [2,1,3,4]

	assert csll.pop_front() == 2
	assert csll.toList() == [1,3,4]
	assert csll.tail.next == csll.head

	assert csll.pop_back() == 4
	assert csll.toList() == [1,3]
	assert csll.tail.next == csll.head

	assert csll.pop_front() == 1
	assert csll.toList() == [3]
	assert csll.tail.next == csll.head

	assert csll.pop_back() == 3
	assert csll.toList() == []

	csll = CircularSLL.fromList([1,2,3,4,5,6])
	assert csll.toList() == [1,2,3,4,5,6]
	assert csll.tail.value == 6
	assert csll.head.value == 1
	assert csll.tail.next == csll.head

