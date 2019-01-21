'''
Implement a LRU cache that supports the 'set()' and 'get()' operations.

  + get(key) - Get the value (will always be positive) of the key if the key exists in the cache, otherwise return -1.

  + put(key, value) - Set or insert the value if the key is not already present. 
    When the cache reached its capacity, it should invalidate the least recently used item before inserting a new item. 

  + get(), set() are O(1)

Solution:
	Use a hash-table that associates cache lookup keys to nodes in a queue based on a doubly linked list.
	The doubly linked list/Queue nodes contain (key, value) ordered by their recent usage.

	+ get(key): find key in hash-table, get the corresponding node in the DLL.
	  fetch node.value to return, since 'key' was recently used, 
	  re-enqueue 'node' back to the back of the DLL/queue.
	  NOTE: The queue is based on a DLL instead of a SLL, so re-enqueues are efficient.

	+ set(key, value):
      if the cache has not reached capacity,
	  add node(key,value) into the queue at the end. 
	  map key to node in the hash-table.
	  Otherwise, 
	    if the cache has reached capacity -> time to flush the least recently entry in the cache.
		the least recently used 'key_' is the one at the front of the DLL/queue.
		dequeue it, remove said 'key_' from the hash-table as well.
		Now, add new (key,value) into the cache as usual.

+---------------------------+
|      +---+                |
|      | a +--------+       |
|      +---+        |       |
+------+ b |      +-v-+   +-v-+   +---+  DLL/Q
       +---+      | a <---+ b <---+ c |
       | c |      | a'+---> b'+---> c'|
   H/T +-+-+      +---+   +---+   ++--+
         |                         ^
         |                         |
         +-------------------------+

'''

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
			item = self.queue.dequeue()
			self.table.pop(item[0]) # remove key from the table

		# Cache has enough capacity to accomodate the new entry
		item = (key, value)

		# Add to the end of the queue
		# Add a reference of the DLL node (encapsulating the item)
		# into the hash table
		self.queue.enqueue(item)
		self.table[key] = self.queue.tail
        
