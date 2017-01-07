
class Node:
	def __init__(self, value):
		self.value = value
		self.next = None

	def __str__(self):
		return str(self.value)


class SLL:
	def __init__(self):
		self.head = None
		self.tail = None
		self.size = 0

	# Insert at front
	def insert(self, value):
		node = Node(value)
		self.size += 1

		node.next = self.head
		self.head = node

		if (not self.tail):
			self.tail = node

	# Append to the rear
	def append(self, value):
		node = Node(value)

		self.size += 1

		self.tail.next = node
		self.tail = node

	def __str__(self):
		sll_str = '[' + str(self.size) + ']: '
		trav = self.head
		while (trav):
			sll_str += str(trav) + ' '
			trav = trav.next

		return sll_str


sll = SLL()
sll.insert(1)
sll.insert(3)
sll.insert(4)
sll.insert(2)
sll.append(1)
sll.append(2)
sll.append(3)

print sll
