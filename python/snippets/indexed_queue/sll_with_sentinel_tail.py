from sll_remove_next import SLL

class SLLWithRemove:
	def __init__(self):
		sentinel = SLL.Node() # sentinel node
		self.head = self.tail = sentinel

	
	def __nonzero__(self):
		# SLL is empty is head == tail/sentinel
		return self.head is not self.tail


	def front(self):
		return self.head.item if self else None

	# Add item to the back of the SLL
	def push_back(self, item):
		# Use the current sentinel tail-node to store the new item
		self.tail.item = item

		# create a new sentinel for the tail node
		node = SLL.Node() 

		self.tail.next = node
		self.tail = node


	# Remove item front of the SLL and return it
	def pop_front(self):
		if not self.head:
			raise ValueError("SLL is empty!")

		item = self.head.item
		self.head = self.head.next
		return item


	# Remove specified `node` from the SLL
	# `node` is assumed to never be the tail-sentinel
	# infact, it's often hidden outside the SLL
	def remove(self, node):
		if node is self.head:
			return self.pop_front()

		item = node.item
		node.item = node.next.item
		node.next = node.next.next

		if node.next is None:
			# `node` is the new sentinel tail node
			self.tail = node
		return item


	# Build a linked-list from a list
	@staticmethod
	def fromList(l):
		s = SLLWithRemove()
		for x in l:
			s.push_back(x)
		return s


	# Build a list from a linked-list
	def toList(self):
		tmp = self.head
		l = []
		while tmp is not self.tail:
			l.append(tmp.item)
			tmp = tmp.next

		return l


if __name__ == '__main__':
	s = SLLWithRemove()
	assert s.front() == None

	s = SLLWithRemove.fromList([1,2,3,4,5])
	assert s.toList() == [1,2,3,4,5]
	s.push_back(10)
	assert s.toList() == [1,2,3,4,5,10]

	node = s.head.next.next.next.next.next
	assert node.item == 10
	assert s.remove(node) == 10
	assert s.toList() == [1,2,3,4,5]

	node = s.head.next.next
	assert node.item == 3
	assert s.remove(node) == 3
	assert s.toList() == [1,2,4,5]

