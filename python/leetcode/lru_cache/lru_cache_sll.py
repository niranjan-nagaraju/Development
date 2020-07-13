'''
https://leetcode.com/problems/lru-cache/

146. LRU Cache

Design and implement a data structure for Least Recently Used (LRU) cache. It should support the following operations: get and put.

get(key) - Get the value (will always be positive) of the key if the key exists in the cache, otherwise return -1.
put(key, value) - Set or insert the value if the key is not already present. When the cache reached its capacity, it should invalidate the least recently used item before inserting a new item.

The cache is initialized with a positive capacity.

Follow up:
Could you do both operations in O(1) time complexity?

Example:

LRUCache cache = new LRUCache( 2 /* capacity */ );

cache.put(1, 1);
cache.put(2, 2);
cache.get(1);       // returns 1
cache.put(3, 3);    // evicts key 2
cache.get(2);       // returns -1 (not found)
cache.put(4, 4);    // evicts key 1
cache.get(1);       // returns -1 (not found)
cache.get(3);       // returns 3
cache.get(4);       // returns 4
'''

'''
Solution Outline:
	1. Use an SLL-based queue with lookup for nodes.
		1.1 Initialize SLL-Queue with a dummy tail so any node can be deleted in O(1) given only its reference
		1.2 The lookup table for nodes contains a mapping for cache keys -> nodes in the queue
	2. put(key, value):
		2.1 if the cache already has the key, move its node to the back marking it as MRU
			Update its value in the node
		2.2 Otherwise.
			if the LRU-cache is at full-capacity: evict a LRU entry from the front of the queue.
			Add the new key to the back of the queue.
	3. get(key):
		3.1 If the key is not found in the lookup table, => return -1
		3.2 Get the node for the key, move it to the back of the queue marking it as MRU
			return its value

Sample run:
	Lookup: {}
	Queue: []
	capacity: 2

	put(1,1)
		lookup(1): keyerror
		Add to queue, update lookup table with the key->node
		Queue: (1,1)
		Lookup: {1:n1}

	put(2,2)
		lookup(2): keyerror
		Add to queue, update lookup table with the key->node
		Queue: n1(1,1) -> n2(2,2)
		Lookup: {1:n1, 2:n2}
	
	get(1):
		lookup(1) -> n1
		val: 1
		Move (1,1) to back of the queue
		Queue: n2(2,2) -> n1(1,1)
		return 1
		
	put(3,3):
		len(Queue) == 2 == capacity
		evict
		Queue front: n2(2,2) -> dequeue (n2)
		Queue: n1(1,1)	
		lookup: {1: n1}
		Enqueue(3,3):
		Queue: n1(1,1) -> n3(3,3)
		lookup: {1: n1, 3: n3}
'''
class SLLQueueWithLookup:
	class Node:
		def __init__(self, item=None):
			self.item = item
			self.next = None

		def __str__(self):
			return str(self.item) if self.item else '<None>'

	def __init__(self):
		# Initialize SLL with a dummy node
		self.head = self.tail = SLLQueueWithLookup.Node()
		self.lookup = {}
		self.num_items = 0
	
	def __str__(self):
		tmp = self.head
		qstr = []
		while tmp:
			qstr.append(str(tmp))
			tmp = tmp.next

		dstr = []
		for k,v in self.lookup.items():
			dstr.append(str(k) + ":" + str(v))
		return str(qstr) + '\n' + str(dstr)


	# Number of items in the SLL-Queue
	def __len__(self):
		return self.num_items

	
	# Add 'key' to the back of the queue
	def enqueue(self, key, value):
		# Create a new dummy node for tail, and append it
		new = SLLQueueWithLookup.Node()
		node = self.tail
		node.next = new

		# Use current dummy tail node to store 'key'
		node.item = (key, value)
		self.lookup[key] = node

		# Make new node the dummy tail node
		self.tail = new
		self.num_items += 1


	# Remove 'key' from the front of the queue and lookup table
	def dequeue(self):
		if self.num_items == 0:
			return

		self.lookup[self.head.item[0]] = None
		self.removeNode(self.head)	
	

	# Remove 'key' and its node from the SLL queue
	def remove(self, key):
		if self.lookup.get(key) is None:
			# key doesn't exist in the cache
			return

		node = self.lookup[key]
		self.lookup[key] = None
		self.removeNode(node)


	# Remove a specified node from the SLL queue
	def removeNode(self, node):
		self.num_items-= 1

		# copy node.next item into node
		# and remove node.next from the list
		node.item = node.next.item
		node.next = node.next.next

		if node.item is not None:
			# node that previously contained item(to delete)
			# now contains next node's item
			# update lookup table
			self.lookup[node.item[0]] = node
		else:  # node.item == None:
			# 'node' was the last 'valid' node in the SLL
			# make it the new dummy tail
			self.tail = node


	# Re-enqueue 'key' to the back of the queue
	# Unsafe: will not check if the queue is empty or
	# if the key doesn't exist in the queue/lookup table
	# Caller is expected to check before calling
	def reEnqueue(self, key, value=None):
		node = self.lookup[key]

		if value is None:
			value = node.item[1]

		if node.next == self.tail:
			# re-enqueuing queue's last node
			# is redundant
			# will place it back at the same place
			# return without doing anything
			node.item = (key, value)
			return

		# Yank node from the SLL queue
		# copy node.next item into node
		# and remove node.next from the list
		yanked = node.next
		node.item = node.next.item
		node.next = node.next.next

		# node that previously contained item(to delete)
		# now contains next node's item
		# update lookup table
		self.lookup[node.item[0]] = node

		# Re-enqueue 'yanked' node at the end of the SLL-Queue
		self.tail.next = yanked
		self.tail.item = (key, value)
		# update lookup table for key
		self.lookup[key] = self.tail

		# Make 'yanked' node the new dummy tail node
		yanked.item = None
		yanked.next = None
		self.tail = yanked


	# Lookup key's node, and return its value
	# None if the key doesn't exist
	def get(self, key):
		node = self.lookup.get(key)
		return (node.item[1] if node is not None else None)



class LRUCache(object):
	def __init__(self, capacity):
		"""
		:type capacity: int
		"""
		self.queue = SLLQueueWithLookup()
		self.capacity = capacity


	def get(self, key):
		"""
		:rtype: int
		"""
		value = self.queue.get(key)
		if value == None:
			return -1
		
		# Re-enqueue 'key' to the back of the queue so it becomes MRU
		self.queue.reEnqueue(key)
		return value


	def put(self, key, value):
		"""
		:type key: int
		:type value: int
		:rtype: nothing
		"""
		# if key already exists, 
		# update with new value and re-enqueue key to the back of the queue
		old_value = self.queue.get(key)
		if (old_value is not None):
			self.queue.reEnqueue(key, value)
			return
			
		if (len(self.queue) == self.capacity):
			# Dequeue least recently used item from the queue
			# Remove it from the table as well
			self.queue.dequeue()

		# Add to the end of the queue
		node = self.queue.enqueue(key, value)


if __name__ == '__main__':
	#2,[put(2,1),put(2,2),get(2),put(1,1),put(4,1),get(2)]
	cache1 = LRUCache(2)
	cache1.put(2,1)
	cache1.put(2,2)
	assert cache1.get(2) == 2
	cache1.put(1,1)
	cache1.put(4,1) # evicts (2,2)
	assert cache1.get(2) == -1

	cache = LRUCache(2)
	cache.put(1,2)
	cache.put(2,3)
	assert(cache.get(1) == 2)
	cache.put(3,4) # Invalidates (2,3)
	assert(cache.get(2) == -1)
	assert(cache.get(3) == 4)
	cache.put(4,5) # Invalidates (1,2)
	assert(cache.get(1) == -1)
	assert(cache.get(4) == 5)
	assert(cache.get(3) == 4)

	cache3 = LRUCache(2)
	cache3.put(1,11)
	cache3.put(2,22)
	assert cache3.get(1) == 11
	cache3.put(3,33) # evicts (2,22)
	assert cache3.get(2) == -1
	cache3.put(4,44) # evicts (1,11)
	assert cache3.get(1) == -1
	assert cache3.get(3) == 33
	assert cache3.get(4) == 44

