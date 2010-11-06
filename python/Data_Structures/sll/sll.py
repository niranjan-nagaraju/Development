
class Node:
	def __init__(self, value):
		self.value = value
		self.next = 0

class SLL:
	def __init__(self):
		self.head = Node(0)

	def insert(self, value):
		node = Node(value)
		node.next = self.head
		self.head = node

	def show(self):
		trav = self.head
		while (trav.next != 0):
			print trav.value,
			trav = trav.next

sll = SLL()
sll.insert(1)
sll.insert(3)
sll.insert(4)
sll.insert(2)

sll.show()
