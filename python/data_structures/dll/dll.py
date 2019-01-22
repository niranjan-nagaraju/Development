from node import *

class DLL(object):
	def __init__(self):
		self.head = None
		self.tail = None
		self.size = 0


	# Length of the DLL, aliases to len(DLL)
	def __len__(self):
		return self.size


	# utility function to push a node to the front of a DLL
	def _push_front_node(self, node):
		self.size += 1
		node.next = self.head

		if self.head:
			self.head.prev = node

		self.head = node
		if (not self.tail):
			self.tail = node


	# utility function to wrap data in a node, and push to the front of a DLL
	def _push_front_data(self, value):
		node = Node(value)
		self._push_front_node(node)


	# Insert at front
	# if a node is sent as a parameter, it is enqueued as-is
	# otherwise, data is wrapped around in a node and enqueued.
	# if a node needs enqueued wrapped around in another node (for whatever reason), 
	#  use _push_front_data() instead
	def push_front(self, data):
		if not isinstance(data, Node):
			node = Node(data)
		self._push_front_node(node)


	# utility function to push a node to the back of a DLL
	def _push_back_node(self, node):
		self.size += 1
		node.prev = self.tail
		if self.tail:
			self.tail.next = node

		self.tail = node
		if not self.head:
			self.head = node


	# utility function to wrap data in a node, and push to the back of a DLL
	def _push_back_data(self, value):
		node = Node(value)
		self._push_back_node(node)


	# Append to the rear
	# if a node is sent as a parameter, it is enqueued as-is
	# otherwise, data is wrapped around in a node and enqueued.
	# if a node needs enqueued wrapped around in another node (for whatever reason), 
	#  use _push_back_data() instead
	def push_back(self, data):
		if not isinstance(data, Node):
			node = Node(data)
		self._push_back_node(node)



	# remove front node and return its data
	def pop_front(self):
		node = self.pop_front_node()
		return node.value


	# remove front node and return it
	def pop_front_node(self):
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


	# remove last node and return its data
	def pop_back(self):
		node = self.pop_back_node()
		return node.value


	# remove last node in the DLL and return it
	def pop_back_node(self):
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


	# Add a new item, next to the specified 'node' in the DLL
	def add_next_to(self, node, item):
		new_node = Node(value=item)
		self.size += 1

		# 'node' is currently the tail
		# Just update node's next and the new node's prev links, and return
		if not node.next:
			new_node.prev = node
			node.next = new_node

			#update tail
			self.tail = new_node
			return

		new_node.next = node.next
		node.next.prev = new_node
		new_node.prev = node
		node.next = new_node


	# Remove node matching 'data' from the DLL
	def remove(self, data):
		trav = self.head
		while trav:
			if trav.value == data:
				return self.removeNode(trav)
			trav = trav.next

		return None


	# Cull the node from a DLL and return it 'sanitized'
	# TODO: Merge into remove() based on isinstance
	def removeNode(self, node):
		if node == self.head:
			return self.pop_front_node()
		elif node == self.tail:
			return self.pop_back_node()
		
		self.size -= 1
		
		node.prev.next = node.next
		node.next.prev = node.prev

		node.next = node.prev = None

		return node


	#[]
	# FIXME: should be returning data, not a reference to a node
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
		dll.push_front(i)
		print dll

	print 'By Index:', dll[0], dll[1], dll[2], dll[3], dll[4]  # [1,2,3,4,5]

	node = dll[2]
	dll.add_next_to(node, 'n')  # [1,2,3,'n', 4,5]
	print dll
	dll.removeNode(node.next) # back to [1,2,3,4,5]
	print dll

	dll.add_next_to(dll.tail, 'tail') # [1,2,3,4,5, 'tail']
	assert(dll.tail.value == 'tail')
	dll.removeNode(dll.tail) # back to [1,2,3,4,5]
	assert(str(dll) == "[5]: 1 2 3 4 5 ")
	
	

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
		print dll.pop_front()
		print dll

	for i in range(1, 6):
		dll.push_back(i)
		print dll

	for i in range(1, 6):
		print dll.pop_back()
		print dll
'''
