from node import *

class DLL:
	def __init__(self):
		self.head = None
		self.tail = None
		self.size = 0

	# Insert at front
	def prepend(self, value):
		node = Node(value)
		self.size += 1

		node.next = self.head

		if self.head:
			self.head.prev = node

		self.head = node

		if (not self.tail):
			self.tail = node


	# Append to the rear		
	def append(self, value):
		node = Node(value)
		self.appendNode(node)


	# Append a node to the rear
	def appendNode(self, node):
		self.size += 1

		node.prev = self.tail

		if self.tail:
			self.tail.next = node

		self.tail = node

		if not self.head:
			self.head = node


	# remove front node and return it
	def removeFront(self):
		if (self.size == 0):
			return None

		self.size -= 1
		tmp = self.head
		self.head = self.head.next

		if self.head:
			self.head.prev = None
		else:
			self.tail = None

		tmp.next = tmp.prev = None
		return tmp

	# remove last node in the DLL and return it
	def removeEnd(self):
		if (self.size == 0):
			return None

		self.size -= 1
		tmp = self.tail
		self.tail = self.tail.prev

		if self.tail:
			self.tail.next = None
		else:
			self.head = None

		tmp.next = tmp.prev = None
		return tmp


	# Cull the node from a DLL and return it 'sanitized'
	def removeNode(self, node):
		if node == self.head:
			return self.removeFront()
		elif node == self.tail:
			return self.removeEnd()
		
		self.size -= 1
		
		node.prev.next = node.next
		node.next.prev = node.prev

		node.next = node.prev = None

		return node

	#[]
	def __getitem__(self, index):
		# for now
		if index < 0:
			return None

		if self.size == 0 or (not index < self.size):
			return None

		i = 0
		trav = self.head
		while i<index:
			trav = trav.next
			i += 1

		return trav

	def __str__(self):
		dll_str = '[' + str(self.size) + ']: '
		trav = self.head
		while (trav):
			dll_str += str(trav) + ' '
			trav = trav.next

		return dll_str


if __name__ == "__main__":
	dll = DLL()

	for i in range(5, 0, -1):
		dll.prepend(i)
		print dll

	print 'By Index:', dll[0], dll[1], dll[2], dll[3], dll[4]

	# Remove node @2
	print 'Removed: from Index 2', dll.removeNode(dll[2])
	print dll

	# Remove node @0
	print 'Removed: from Index 0', dll.removeNode(dll[0])
	print dll

	# Remove node @$
	print 'Removed: from Index $', dll.removeNode(dll[2])
	print dll

'''
	for i in range(5, 0, -1):
		print dll.removeFront()
		print dll

	for i in range(1, 6):
		dll.append(i)
		print dll

	for i in range(1, 6):
		print dll.removeEnd()
		print dll
'''
