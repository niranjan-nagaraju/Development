'''
Implement LRU cache
https://leetcode.com/problems/lru-cache/

All data structures inline
'''

class Node:
	def __init__(self, value):
		self.value = value
		self.prev = None
		self.next = None

	def __str__(self):
		return str(self.value)


class DLL:
	def __init__(self):
		self.head = None
		self.tail = None
		self.size = 0

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


	def __str__(self):
		dll_str = '[' + str(self.size) + ']: '
		trav = self.head
		while (trav):
			dll_str += str(trav) + ' '
			trav = trav.next

		return dll_str



# Queue using a DLL
class Queue:
	def __init__(self):
		self.dll = DLL()

	def size(self):
		return self.dll.size

	# enqueue and return DLL node (will be the tail)
	def enqueue(self, value):
		self.dll.append(value)
		return self.dll.tail

	def enqueueNode(self, node):
		self.dll.appendNode(node)
		return self.dll.tail

	# dequeue and return the DLL node (will be the previous head)
	def dequeue(self):
		return self.dll.removeFront()

	def reEnqueueNode(self, node):
		# if the node is already at the 
		# back of the queue, 
		# removing it and adding back to the same place
		# is wasteful
		if self.dll.tail == node:
			return

		tmp = self.dll.removeNode(node)
		self.enqueueNode(node)


	def __str__(self):
		return self.dll.__str__()



class LRUCache(object):

	def __init__(self, capacity):
		"""
		:type capacity: int
		"""
		self.queue = Queue()
		self.table = {}
		self.capacity = capacity


	def get(self, key):
		"""
		:rtype: int
		"""
		# Lookup key in the table
		if self.table.has_key(key):
			node = self.table[key]
		else:
			return -1

		# Remove the node from the queue and 
		# enqueue it back to the queue to mark it
		# as MRU
		self.queue.reEnqueueNode(node)

		return node.value[1]


	def set(self, key, value):
		"""
		:type key: int
		:type value: int
		:rtype: nothing
		"""

		# if key already exists, 
		# update with new value and return
		if (self.table.has_key(key)):
			node = self.table[key]
			node.value = (key, value)
			self.get(key)
			return
			

		if (self.queue.size() == self.capacity):
			# Dequeue least recently used item from the queue
			# Remove it from the table as well
			node = self.queue.dequeue()
			self.table.pop(node.value[0])

		item = (key, value)

		# Add to the end of the queue
		# Add a reference of the DLL node (encapsulating the item)
		# into the hash table
		node = self.queue.enqueue(item)
		self.table[key] = node


if __name__ == '__main__':
	#2,[set(2,1),set(2,2),get(2),set(1,1),set(4,1),get(2)]

	cache1 = LRUCache(2)
	cache1.set(2,1)
	cache1.set(2,2)
	print cache1.queue

	cache = LRUCache(2)
	cache.set(1,2)
	cache.set(2,3)
	assert(cache.get(1) == 2)
	cache.set(3,4) # Invalidates (2,3)
	assert(cache.get(2) == -1)
	assert(cache.get(3) == 4)
	cache.set(4,5) # Invalidates (1,2)
	assert(cache.get(1) == -1)
	assert(cache.get(4) == 5)
	assert(cache.get(3) == 4)
	print cache.queue
