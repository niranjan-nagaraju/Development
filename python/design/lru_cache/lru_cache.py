import sys
sys.path.append("../../")
from data_structures.dll.queue import Queue
from data_structures.dll.node import Node


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

		try:
			# Lookup key in the table
			node = self.table[key]
		except KeyError:
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

		if (self.queue.length() == self.capacity):
			# Dequeue least recently used item from the queue
			# Remove it from the table as well
			node = self.queue.dequeue()
			self.table.pop(node.value[0])

		item = (key, value)

		# Add to the end of the queue
		# Add a reference of the DLL node (encapsulating the item)
		# into the hash table
		self.queue.enqueue(item)
		self.table[key] = self.queue.tail
        
