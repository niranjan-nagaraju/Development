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


Test runs:
	+ Initial state: 
		Cache: hashtable: {}, queue:[], capacity: 3
	+ set(a,1):
		Cache: hashtable: {a: node0}, queue: {1: [(a,1)]}
	+ set(b,2):
		Cache: hashtable: {a: node0, b: node1}, queue: {2: [(a,1), (b,2)]}
	+ set(c,3):
		Cache: hashtable: {a: node0, b: node1, c: node2}, queue: {3: [(a,1), (b,2), (c,3)]}
	+ get(b):
		hashtable[b] <- node1
		queue[node1] <- (b,2) == 2
		updated Cache: hashtable: {a: node0, b: node1, c: node2}, queue: {3: [(a,1), (c,3), (b,2)]}
	+ set(d,4):
		Cache: hashtable: {a: node0, b: node1, c: node2}, queue: {3: [(a,1), (c,3), (b,2)]}
		-> remove (a,1) from queue, and hashtable
		Cache: hashtable: {b: node1, c: node2}, queue: {2: [(c,3), (b,2)]}
		-> Insert (d,4) at end
		Cache: hashtable: {b: node1, c: node2, d: node3}, queue: {3: [(c,3), (b,2), (d,4)]
	+ get(a):
		-1
	+ get(c):
		Cache: hashtable: {b: node1, c: node2, d: node3}, queue: {3: [(c,3), (b,2), (d,4)]
		hashtable[c] <- node2
		queue[node2] <- (c,3) == 1
		updated Cache: hashtable: {b: node1, c: node2, d: node3}, queue: {3: [(b,2), (d,4), (c,3)]
	+ get(b):
		Cache: hashtable: {b: node1, c: node2, d: node3}, queue: {3: [(b,2), (d,4), (c,3)]
		hashtable[b] <- node1
		queue[node1] <- (b,2) == 2
		updated Cache: hashtable: {b: node1, c: node2, d: node3}, queue: {3: [(d,4), (c,3), (b,2)]
	+ set(d,5):
		Cache: hashtable: {b: node1, c: node2, d: node3}, queue: {3: [(d,4), (c,3), (b,2)]
		-> key 'd' already exists, update new value
		hashtable[d] <- node3,
		node3.value (d,5)
		-> move 'd' to the end of the queue as it is no longer the LRU entry
		updated Cache: hashtable: {b: node1, c: node2, d: node3}, queue: {3: [(c,3), (b,2), (d,4)]
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

		# shortcuts to get (key,value) from a pair item
		self.key = lambda item: item[0]
		self.value = lambda item: item[1]


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

		return self.value(node.value)


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
			# mark current node as MRU by moving it to the back of the queue
			self.queue.reEnqueueNode(node)
			return

		if (self.queue.length() == self.capacity):
			# Dequeue least recently used item from the queue
			# Remove it from the table as well
			item = self.queue.dequeue()
			self.table.pop(self.key(item)) # remove key from the table

		# Cache has enough capacity to accomodate the new entry
		item = (key, value)

		# Add to the end of the queue
		# Add a reference of the DLL node (encapsulating the item)
		# into the hash table
		self.queue.enqueue(item)
		self.table[key] = self.queue.tail


	# [] operator for read
	# a = cache['x']
	def __getitem__(self, key):
		return self.get(key)

	# [] operator for write
	# cache['x'] = 'blah'
	def __setitem__(self, key, value):
		return self.set(key, value)



def testcase1():
	cache1 = LRUCache(2)
	cache1.set(2,1)
	cache1.set(2,2)
	assert(str(cache1.queue) == "[1]: (2, 2) ")

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
	assert(str(cache.queue) == "[2]: (4, 5) (3, 4) ")


# Test example from test run in solution description
def testcase2():
	cache = LRUCache(3)
	assert(len(cache.table) == len(cache.queue) == 0)

	cache.set('a', 1)
	assert(len(cache.table) == len(cache.queue) == 1)
	node0 = cache.queue.frontNode()
	assert(cache.table['a'] == node0)
	assert(node0.value == ('a', 1))

	cache['b'] = 2 # same as cache.set('b', 2)
	assert(len(cache.table) == len(cache.queue) == 2)
	node1 = node0.next
	assert(cache.table['b'] == node1)
	assert(node1.value == ('b', 2))

	cache['c'] = 3 # same as cache.set('c', 3)
	assert(len(cache.table) == len(cache.queue) == 3)
	node2 = node1.next
	assert(cache.table['c'] == node2)
	assert(node2.value == ('c', 3))

	assert(cache.get('b') == 2)
	assert(cache.table['b'] == node1)
	assert(node1.value == ('b', 2))
	assert(cache.queue.back() == ('b', 2))
	assert(str(cache.queue) == "[3]: ('a', 1) ('c', 3) ('b', 2) ")

	cache['d'] = 4 # same as cache.set('d', 4) -> invalidates (a,1)
	assert(len(cache.table) == len(cache.queue) == 3)
	node3 = cache.queue.lastNode()
	assert(cache.table['d'] == node3)
	assert(node3.value == ('d', 4))
	assert(cache.table.has_key('a') == False)
	assert(str(cache.queue) == "[3]: ('c', 3) ('b', 2) ('d', 4) ")

	assert(cache['a'] == -1)

	assert(cache['c'] == 3)
	assert(len(cache.table) == len(cache.queue) == 3)
	assert(str(cache.queue) == "[3]: ('b', 2) ('d', 4) ('c', 3) ")

	assert(cache['b'] == 2)
	assert(len(cache.table) == len(cache.queue) == 3)
	assert(str(cache.queue) == "[3]: ('d', 4) ('c', 3) ('b', 2) ")

	cache['d'] = 5 # same as cache.set('d', 5) -> overwrite 'd', and update its recency
	assert(len(cache.table) == len(cache.queue) == 3)
	assert(str(cache.queue) == "[3]: ('c', 3) ('b', 2) ('d', 5) ")



if __name__ == '__main__':
	testcase1()
	testcase2()

