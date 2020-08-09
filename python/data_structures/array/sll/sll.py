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


	# Return a free node from the available list
	# if there are any
	# Raises MemoryError if the entire capacity of the array is currently allocated
	def getNode(self):
		if self.available_list == -1:
			raise MemoryError("Linked list is at capacity")

		node = self.available_list
		self.available_list = self._array[self.available_list].next
		self._array[node].next = -1
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
		node = self.getNode()
		self._array[node].data = data

		if self.allocated_list == -1:
			self.allocated_list = self.allocated_tail = node
			return

		self._array[self.allocated_tail].next = node
		self.allocated_tail = node


	# Insert a node at the front to the SLL
	def push_front(self, data):
		# get a freenode from the available list
		node = self.getNode()
		self._array[node].data = data

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




if __name__ == '__main__':
	sll = SLL(10)
	assert sll.allocated_list == -1 # Empty SLL
	assert len(sll) == 0

	sll.push_back(78)
	sll.push_back(10)
	sll.push_back(41)
	sll.push_back(36)
	sll.push_back(21)

	assert str(sll) == "[5]: 78 -> 10 -> 41 -> 36 -> 21 -> "
	assert len(sll) == 5

	sll.push_front(1)
	sll.push_front(3)

	assert str(sll) == "[7]: 3 -> 1 -> 78 -> 10 -> 41 -> 36 -> 21 -> "

	sll.push_back(8)
	sll.push_back(9)
	sll.push_back(10)
	assert str(sll) == "[10]: 3 -> 1 -> 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> 9 -> 10 -> "

	assert sll.available_list == -1 # at capacity

	failed_push = False
	try: 
		sll.push_back(10)
	except MemoryError:
		failed_push = True
	assert failed_push == True

	failed_push = False
	try: 
		sll.push_front(11)
	except MemoryError:
		failed_push = True
	assert failed_push == True

	assert sll.pop_front() == 3
	assert str(sll) == "[9]: 1 -> 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> 9 -> 10 -> "

	assert sll.pop_front() == 1
	assert str(sll) == "[8]: 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> 9 -> 10 -> "

	assert sll.pop_back() == 10
	assert str(sll) == "[7]: 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> 9 -> "

	assert sll.pop_back() == 9
	assert str(sll) == "[6]: 78 -> 10 -> 41 -> 36 -> 21 -> 8 -> "

	assert sll.pop_back() == 8
	assert str(sll) == "[5]: 78 -> 10 -> 41 -> 36 -> 21 -> "

	l = [78, 10, 41, 36, 21]
	for i in xrange(5):
		assert len(sll) == 5-i
		assert sll.pop_front() == l[i]

	assert len(sll) == 0
	assert sll.allocated_list == -1
	assert sll.allocated_tail == -1
