'''
Removing a node, `n2`, given a reference to  its previous node, `n1`
'''


class SLL:
	class Node:
		def __init__(self, item=None):
			self.item = item
			self.next = None

	def __init__(self):
		self.head = None

	def removeNext(self, n1):
		# n1 and n2 are assumed to be non-empty
		# for the sake of simplicity
		n2 = n1.next
		n1.next = n2.next

	def remove(self, node):
		if node is self.head:
			self.head = self.head.next
			return
		node.item = node.next.item
		self.removeNext(node)

	# Build a linked-list from a list
	@staticmethod
	def fromList(l):
		s = SLL()
		s.head = SLL.Node(l[0])
		tmp = s.head
		for x in l[1:]:
			node = SLL.Node(x)
			tmp.next = node
			tmp = tmp.next
		return s


	# Build a list from a linked-list
	def toList(self):
		tmp = self.head
		l = []
		while tmp:
			l.append(tmp.item)
			tmp = tmp.next

		return l



if __name__ == '__main__':
	s = SLL.fromList([1,2,3,4,5])
	node = s.head.next.next
	assert s.toList() == [1,2,3,4,5]
	assert node.item == 3
	s.removeNext(node)
	assert s.toList() == [1,2,3,5]

	s.remove(s.head)
	assert s.toList() == [2,3,5]
	s.remove(s.head.next)
	assert s.toList() == [2,5]
