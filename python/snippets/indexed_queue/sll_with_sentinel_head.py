from sll_remove_next import SLL

class SLLWithSentinel:
	def __init__(self):
		sentinel = SLL.Node() # sentinel node
		self.head = self.tail = sentinel

	def front(self):
		'''
		Actual head is the second node
		'''
		return self.head.next.item if self.head.next else None

	def push_back(self, item):
		node = SLL.Node(item)
		self.tail.next = node
		self.tail = node

	# Build a linked-list from a list
	@staticmethod
	def fromList(l):
		s = SLLWithSentinel()
		for x in l:
			s.push_back(x)
		return s


	# Build a list from a linked-list
	def toList(self):
		tmp = self.head.next
		l = []
		while tmp:
			l.append(tmp.item)
			tmp = tmp.next

		return l


if __name__ == '__main__':
	s = SLLWithSentinel()
	assert s.front() == None

	s = SLLWithSentinel.fromList([1,2,3,4,5])
	assert s.toList() == [1,2,3,4,5]
	s.push_back(10)
	assert s.toList() == [1,2,3,4,5,10]

	
