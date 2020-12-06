'''
https://leetcode.com/problems/lru-cache/

146. LRU Cache

Design and implement a data structure for Least Recently Used (LRU) cache. It should support the following operations: get and set.

get(key) - Get the value (will always be positive) of the key if the key exists in the cache, otherwise return -1.
set(key, value) - Set or insert the value if the key is not already present. When the cache reached its capacity, it should invalidate the least recently used item before inserting a new item.

The cache is initialized with a positive capacity.

Follow up:
Could you do both operations in O(1) time complexity?

Example:

LRUCache cache = new LRUCache( 2 /* capacity */ );

cache.set(1, 1);
cache.set(2, 2);
cache.get(1);       // returns 1
cache.set(3, 3);    // evicts key 2
cache.get(2);       // returns -1 (not found)
cache.set(4, 4);    // evicts key 1
cache.get(1);       // returns -1 (not found)
cache.get(3);       // returns 3
cache.get(4);       // returns 4
'''

'''
Solution Outline:
	0. Use an Indexed-queue made from a regular SLL, with a lookup for nodes.
	1. set(key, value):
		1.1 if the cache already has the key, move its node to the back marking it as MRU
			Update its value in the node
		1.2 Otherwise.
			if the LRU-cache is at full-capacity: evict a LRU entry from the front of the queue.
			Add the new key to the back of the queue.
	2. get(key):
		2.1 If the key is not found in the lookup table, => return -1
		2.2 Get the node for the key, move it to the back of the queue marking it as MRU
			return its value
		3.2 A regular SLL-based Queue would do as we don't have to remove the tail node.
			3.2.1 A node is only deleted to be re-enqueued to the back of the queue.
			3.2.2 Re-enqueing a tail node is a NO-OP, needn't involve removing tail node as it'll be redundant.
		3.3 Use the `copy next node's contents to current to delete curent node in O(1)` trick to delete a node using its reference.
'''


from data_structures.sll.indexed_queue_regular import IndexedQueue

class LRUCache(object):
	def __init__(self, capacity):
		"""
		:type capacity: int
		"""
		self.queue = IndexedQueue()
		self.capacity = capacity


	def get(self, key):
		"""
		:rtype: int
		"""
		item = self.queue.get(key)
		if not item:
			return -1
	
		# Re-enqueue 'key' to the back of the queue so it becomes MRU
		self.queue.reEnqueue(key)
		return item


	def set(self, key, value):
		"""
		:type key: int
		:type value: int
		:rtype: nothing
		"""

		# if key already exists, 
		# update with new value and re-enqueue key to the back of the queue
		item = self.queue.get(key)
		if item is not None:
			self.queue.reEnqueue(key, value)
			return
			
		if (len(self.queue) == self.capacity):
			# Dequeue least recently used item from the queue
			# Remove it from the table as well
			self.queue.dequeue()

		# Add to the end of the queue
		self.queue.enqueue(key, value)


if __name__ == '__main__':
	#2,[set(2,1),set(2,2),get(2),set(1,1),set(4,1),get(2)]
	cache1 = LRUCache(2)
	cache1.set(2,1)
	cache1.set(2,2)
	assert cache1.get(2) == 2
	cache1.set(1,1)
	cache1.set(4,1) # evicts (2,2)
	assert cache1.get(2) == -1

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

	cache3 = LRUCache(2)
	cache3.set(1,11)
	cache3.set(2,22)
	assert cache3.get(1) == 11
	cache3.set(3,33) # evicts (2,22)
	assert cache3.get(2) == -1
	cache3.set(4,44) # evicts (1,11)
	assert cache3.get(1) == -1
	assert cache3.get(3) == 33
	assert cache3.get(4) == 44

	cache = LRUCache(5)
	cache.set(1, 101)
	cache.set(2, 202)
	cache.set(1, 111)
	cache.set(3, 303)
	cache.set(4, 404)
	cache.set(5, 505)
	assert cache.get(1) == 111
	cache.set(6, 616) # invalidates 2
	assert cache.get(2) == -1

