'''
A SLL implemented using regular arrays

SLL corresponding to
    78 -> 10 -> 41 -> 36 -> 21
is represented below

          | Index | Node | Node |
          |       | data | next |
          |-------+------+------|
          |     0 |   10 |    7 |
          |     1 |      |      |
          |     2 |   36 |    9 |
          |     3 |      |      |
          |     4 |      |      |
  head -> |     5 |   78 |    0 |
          |     6 |      |      |
          |     7 |   41 |    2 |
          |     8 |      |      |
          |     9 |   21 |   -1 |
          |    10 |      |      |

The underlying array for the SLL contains two disjoint lists
   1. Available-list: contains a list of nodes that are available
   2. Allocated-list: contains a list of nodes that are currently in use
'''

class SLL(object):
	class Node(object):
		def __init__(self, data=None, next=-1):
			self.data = data
			# next index == -1 implies there's no next link
			self.next = next

		def __repr__(self):
			return str(self)

		def __str__(self):
			return str((self.data, self.next))


	def __init__(self, capacity):
		self.capacity = capacity
		self._allocated = 0

		# Initially all nodes are available
		# chain them one-after-another sequentially
		# into an available list
		self._array = [SLL.Node(None, i+1) for i in xrange(self.capacity)]
		self._array[-1].next = -1 # Tail of the available list
			
		self.available_list = 0  # Index 0 is head of the available list
		self.allocated_list = -1 # Allocated list is empty
		self.allocated_tail = -1 # Allocated list is empty => tail: None


	def __len__(self):
		return self._allocated


	def __str__(self):
		lStr = '[{}]: '.format(len(self))
		head = self.allocated_list
		while head != -1:
			lStr += str(self._array[head].data) + " -> "
			head = self._array[head].next
		return lStr


	# Return a free node, initialized to 'data' from the available list.
	# if there are any
	# Raises MemoryError if the entire capacity of the array is currently allocated
	def getNode(self, data):
		if self.available_list == -1:
			raise MemoryError("Linked list is at capacity")

		node = self.available_list
		self.available_list = self._array[self.available_list].next
		self._array[node].next = -1
		self._array[node].data = data
		self._allocated += 1

		return node


	# Add a node back to the available list
	def freeNode(self, node):
		self._allocated -= 1

		# blank data corresponding to the 'freed' node
		# so all the nodes in the available list
		# are blank signifying they are all re-usable containers
		self._array[node].data = None
		self._array[node].next = self.available_list
		self.available_list = node


	# Insert a node to the end of the SLL
	def push_back(self, data):
		# get a freenode from the available list
		node = self.getNode(data)

		if self.allocated_list == -1:
			self.allocated_list = self.allocated_tail = node
			return

		self._array[self.allocated_tail].next = node
		self.allocated_tail = node


	# Insert a node at the front to the SLL
	def push_front(self, data):
		# get a freenode from the available list
		node = self.getNode(data)

		self._array[node].next = self.allocated_list
		self.allocated_list = node

		if self.allocated_tail == -1:
			# First node being added to the SLL
			# update tail
			self.allocated_tail = node


	# Remove a node from the front of the SLL
	def pop_front(self):
		if self.allocated_list == -1:
			# Nothing to pop, list is empty
			return None

		node = self.allocated_list
		data = self._array[node].data
		self.allocated_list = self._array[self.allocated_list].next
		if self.allocated_list == -1:
			self.allocated_tail = -1
		self.freeNode(node)
		return data


	# Remove a node from the end of the SLL
	def pop_back(self):
		if self.allocated_list == -1:
			# Nothing to pop, list is empty
			return None

		node = self.allocated_list
		while self._array[node].next != self.allocated_tail:
			node = self._array[node].next

		data = self._array[self.allocated_tail].data
		self.freeNode(self.allocated_tail)

		# There's only one node in the SLL
		if node == self.allocated_list:
			self.allocated_tail = self.allocated_list = -1
		else:
			self._array[node].next = -1
			self.allocated_tail = node
		return data



	# Place 'data' in the SLL in its rightful place
	# Uses cmp(data, x) {x: for each item in the SLL}
	# Inserting only using 'place()' into the SLL will leave the SLL sorted
	def place(self, data):
		# get a freenode from the available list
		node = self.getNode(data)

		if self.allocated_list == -1:
			self.allocated_list = self.allocated_tail = node
			return

		if data < self._array[self.allocated_list].data:
			# current data is < everything in the SLL
			self._array[node].next = self.allocated_list
			self.allocated_list = node
			return

		if data >= self._array[self.allocated_tail].data:
			# current data is > everything in the SLL
			self._array[self.allocated_tail].next = node
			self.allocated_tail = node
			return

		tmp = self.allocated_list
		prev = None
		while tmp != -1 and self._array[tmp].data <= data:
			prev = tmp
			tmp = self._array[tmp].next

		# At this point, We have found a rightful place to insert current node
		# prev is node after which 'data' needs to be inserted
		self._array[prev].next = node
		self._array[node].next = tmp






