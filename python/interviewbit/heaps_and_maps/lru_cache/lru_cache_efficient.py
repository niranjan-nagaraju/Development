#encoding: utf-8
'''
https://www.interviewbit.com/problems/lru-cache/

LRU Cache

Design and implement a data structure for LRU (Least Recently Used) cache. It should support the following operations: get and set.

get(key) -
	Get the value (will always be positive) of the key if the key exists in the cache, otherwise return -1.
set(key, value) -
	Set or insert the value if the key is not already present. When the cache reaches its capacity, it should invalidate the least recently used item before inserting the new item.

The LRU Cache will be initialized with an integer corresponding to its capacity. Capacity indicates the maximum number of unique keys it can hold at a time.

Definition of ¿least recently used¿ : An access to an item is defined as a get or a set operation of the item.
¿Least recently used¿ item is the one with the oldest access time.

NOTE: If you are using any global variables, make sure to clear them in the constructor. 

Example :
Input : 
         capacity = 2
         set(1, 10)
         set(5, 12)
         get(5)        returns 12
         get(1)        returns 10
         get(10)       returns -1
         set(6, 14)    this pushes out key = 5 as LRU is full. 
         get(5)        returns -1 

'''


'''
Solution Outline:
	1. Use an SLL-based queue with lookup for nodes.
		1.1 A regular SLL would do as we don't have to remove the tail node.
			1.1.1 Re-enqueing a tail node is a NOOP, needn't involve removing tail node as it'll be redundant.
			1.1.2 Dequeueing a tail node will happen when we are tearing down the cache, 
					But this dequeue-ing happens sequentially, traversing from the head.
		1.2 The lookup table for nodes contains a mapping for cache keys -> nodes in the queue
	2. set(key, value):
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

	set(1,1)
		lookup(1): keyerror
		Add to queue, update lookup table with the key->node
		Queue: (1,1)
		Lookup: {1:n1}

	set(2,2)
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
		
	set(3,3):
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
			return str(self.item)


	def __init__(self):
		# Initialize dummy head so enqueue/dequeue needn't check (for head==None)
		# slightly optimizes inserts
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
		node = SLLQueueWithLookup.Node((key, value))
		
		self.tail.next = node
		self.tail = node

		self.lookup[key] = node
		self.num_items += 1



	# Remove 'key' from the front of the queue and lookup table
	def dequeue(self):
		if self.num_items == 0:
			return

		first = self.head.next
		self.lookup[first.item[0]] = None
	
		self.num_items-= 1
		self.head.next = first.next

		# Dequeued node is the head node
		if self.tail == first:
			self.tail = self.head




	# Re-enqueue 'key' to the back of the queue
	# Unsafe: will not check if the queue is empty or
	# if the key doesn't exist in the queue/lookup table
	# Caller is expected to check before calling
	def reEnqueue(self, key, value=None):
		node = self.lookup[key]

		if value is None:
			value = node.item[1]

		if node == self.tail:
			# re-enqueuing queue's last node
			# is redundant
			# update value if it has changed
			# return without removing node
			node.item = (key, value)
			return

		# Yank node's next from the SLL queue
		# copy node.next item into node
		# and remove node.next from the list
		yanked = node.next
		node.item = yanked.item
		node.next = yanked.next

		yanked.next = None

		# yanked node was tail
		# update tail to previous node
		if yanked == self.tail:
			self.tail = node

		# node that previously contained item(to delete)
		# now contains next node's item
		# update lookup table
		self.lookup[node.item[0]] = node

		# Re-enqueue 'yanked' node at the end of the SLL-Queue
		self.tail.next = yanked
		yanked.item = (key, value)
		self.tail = yanked
		# update lookup table for key
		self.lookup[key] = yanked



	# Lookup key's node, and return its value
	# None if the key doesn't exist
	def get(self, key):
		return self.lookup.get(key)





class LRUCache:
	# @param capacity, an integer
	def __init__(self, capacity):
		self.queue = SLLQueueWithLookup()
		self.capacity = capacity        

	# @return an integer
	def get(self, key):
		node = self.queue.get(key)
		if not node:
			return -1
	
		value = node.item[1]

		# Re-enqueue 'key' to the back of the queue so it becomes MRU
		self.queue.reEnqueue(key)
		return value        


	# @param key, an integer
	# @param value, an integer
	# @return nothing
	def set(self, key, value):
		# if key already exists, 
		# update with new value and re-enqueue key to the back of the queue
		node = self.queue.get(key)
		if (node is not None):
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

