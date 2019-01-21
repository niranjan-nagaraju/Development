'''
Implement a LFU cache that supports the 'set()' and 'get()' operations.

  + get(key) - Get the value (will always be positive) of the key if the key exists in the cache, otherwise return -1.

  + put(key, value) - Set or insert the value if the key is not already present. 
    When the cache reached its capacity, it should invalidate the least frequently used item before inserting a new item. 

  + get(), set() are O(1)

Solution:
	Use a doubly-linked list of lists, each node in the top-level list contains a frequency list, which in turn contains (key, value) pairs
	corresponding to the cache entries.
	The top-level DLL is ordered by frequency.

	e.g. (3 frequency lists, list #1 represents 0 frequency,  list #2 represents frequency 1, list #2 represents frequency 3
	     NOTE: there can be gaps in frequencies, in the example, no entries exist with a used-frequency of 2,
		 Each frequency list in turn contains entries with the corresponding used-frequency)
	[3:
	 0, [3: (a,1), (b,2), (c,3)]
	 1, [1: (d,4)]
	 3, [2: (e,5), (f,6)]
	]

	Use a hash-table that associates cache lookup keys to nodes in a list of (frequency) lists.
	Hash-table entries store [key] -> (top-level node containing frequency list, fl_node, node inside frequency list, node)

	+ get(key): 
	  find key in hash-table, (fl_node, node)
	  get the corresponding node in the top-level list, 'fl_node'
	  get the actual 'node' containing (key, value)  -> capture 'value' to be returned.
	  using key.
	  To update current (key, value) entry's frequency, 
		fl_node.frequency contains the current usage-frequency of the 'key' entry.
		fetch frequency list 'fl' from fl_node.list
		check to see if (frequency + 1) is the frequency of the next node (fl_node.next) -> 
		  if not, this is the first item to be added with that frequency => so insert a new node (frequency+1, empty list)
		Remove (key, value) from current frequency list and add it to next node's frequency list with (frequency+1) 
		(NOTE: The relative order of the items within a frequency list doesn't matter, 
		  they all have the same usage frequency and needn't be distinguished between each other)
		(ALSO NOTE: when we remove the last entry from a frequency list, the list (currently empty), and the node containing it 
		is removed from the top-level list)


	+ set(key, value):
	  New entries added to the cache start with 0 frequency, 
	  and as such can be the ripe candidate for culling the next time we need to remove an entry.
      if the cache has not reached capacity,
	    add node(key,value) into the frequency list at node 0.
		if the first node does not represent frequency 0 (cache.head.value.frequency is not 0)
		  then make a new node with freq 0, and an empty list first
		map [key]  to (node containing freq 0, node)
	  Otherwise, 
	    If the cache has reached capacity -> time to flush the least frequently entry in the cache.
		the least frequently used 'key_' is the one at the front of the top-level DLL.
		remove  node(key_) from node0.list, remove said 'key_' from the hash-table as well.
		    if node0 becomes 'empty' (ie node0.list is empty) -> remove node0 as well.
		Now, add new (key,value) into the cache as usual.
'''

import sys
sys.path.append("../../")
from data_structures.dll.node import Node


class LFUCache(object):

	def __init__(self, capacity):
		"""
		:type capacity: int
		"""
		self.queue = Queue()
		self.table = {}
		self.capacity = capacity


	# TODO: Placeholder: implement later
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


	# TODO: Placeholder: implement later
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
        
