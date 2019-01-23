'''
Implement a LFU cache that supports the 'set()' and 'get()' operations.

  + get(key) - Get the value (will always be positive) of the key if the key exists in the cache, otherwise return -1.

  + put(key, value) - Set or insert the value if the key is not already present. 
    When the cache reached its capacity, it should invalidate the least frequently used item before inserting a new item. 

  + get(), set() are O(1)

Solution:
	Use a doubly-linked list of queues(based on DLLs again), each node in the top-level list contains a queue, 
	which in turn contains (key, value) pairs corresponding to the cache entries.
	The top-level DLL is ordered by frequency.

	e.g. (3 frequency queues, queue #1 represents 0 frequency,  queue #2 represents frequency 1, queue #2 represents frequency 3
	     NOTE: there can be gaps in frequencies, in the example, no entries exist with a used-frequency of 2,
		 Each frequency queue in turn contains entries with the corresponding used-frequency)
	[3:
	 0, [3: (a,1), (b,2), (c,3)]
	 1, [1: (d,4)]
	 3, [2: (e,5), (f,6)]
	]

	Use a hash-table that associates cache lookup keys to nodes in a list of (frequency) queues.
	Hash-table entries store [key] -> (top-level node containing frequency list of queues, 'fl_node', node inside frequency queue, 'node')

	+ get(key): 
	  find key in hash-table, (fl_node, node)
	  get the corresponding node in the top-level list, 'fl_node'
	  get the actual 'node' containing (key, value)  -> capture 'value' to be returned.
	  using key.
	  To update current (key, value) entry's frequency, 
		fl_node.frequency contains the current usage-frequency of the 'key' entry.
		fetch frequency queue 'fl' from fl_node.queue
		check to see if (frequency + 1) is the frequency of the next node (fl_node.next) -> 
		  if not, this is the first item to be added with that frequency => so insert a new node (frequency+1, empty queue)
		Remove (key, value) from current frequency queue and add it to next node's frequency queue with (frequency+1) 
		(NOTE: The relative order of the items within a frequency queue is based on their recency of access
		  so while invalidating an entry in a certain frequency queue, 
		  the one that was least-recently-used (LRU) will be considered for evicting from the cache.
		(ALSO NOTE: when we remove the last entry from a frequency queue, the queue (currently empty), and the node containing it 
		is removed from the top-level list)


	+ set(key, value):
	  New entries added to the cache start with 0 frequency, 
	  and as such can be the ripe candidate for culling the next time we need to remove an entry.
	  if the cache has not reached capacity,
	    enqueue node(key,value) into the frequency queue at node 0.
		if the first node does not represent frequency 0 (cache.head.value.frequency is not 0)
		  then make a new node with freq 0, and an empty queue first
		map [key]  to (node containing freq 0, node)
	  Otherwise, 
	    If the cache has reached capacity -> time to flush the least frequently entry in the cache.
		the least frequently used 'key_' is the front node in the queue found in the front of the top-level list of queues.
		dequeue  node(key_) from node0.queue, remove said 'key_' from the hash-table as well.
		    if node0 becomes 'empty' (ie node0.queue is empty) -> remove node0 as well.
		Now, add new (key,value) into the cache as usual.
'''

import sys
sys.path.append("../../")
from data_structures.dll.dll import DLL
from data_structures.dll.queue import Queue


'''
A queue of cache entries with a usage frequency,'f'
Each FrequencyQueue object contains a frequency value, 
and a queue that stores entries that have the same usage-frequency
denoted by the queue's frequency value.

The entries within a frequency queue are ordered by LRU
so the front of the queue always contains the LRU entry for the
frequency corresponding to this queue.

The queue is based off a DLL to facilitate O(1) re-enqueue and 
remove (a specific node) in the queue.

FrequencyQueue is a derived class of Queue as it is essentially a Queue 
with an additional 'frequency' element tacked on
'''
class FrequencyQueue(Queue):
	def __init__(self, frequency=0):
		Queue.__init__(self)
		self.frequency = frequency


	def __str__(self):
		return "%d, %s" %(self.frequency, Queue.__str__(self))



'''
Top-level List of frequency queues
The list is implemented using a DLL, each item in the list
will be a frequency queue object

FrequencyQueuesList derives from DLL, carrying forward DLL's list implementation
and adding methods assuming items are specific to the FrequencyQueue type 
'''
class FrequencyQueuesList(DLL):
	def __init__(self):
		# Call base class DLL's constructor
		DLL.__init__(self)

		# Helper functions to fetch queue and frequency 
		# given a FrequencyQueuesList node
		self.fqueue = lambda fql_node: fql_node.value
		self.frequency = lambda fql_node: fql_node.value.frequency


	# return a multi-line string containing each frequency queue per line in the list
	def __str__(self):
		lstr = "[%d:\n" %(self.size)
		trav = self.head
		while trav:
			lstr += "%s\n" %(trav.value)
			trav = trav.next
		return lstr + ']'


	# LFU queue of items in the list
	def front(self):
		return self.head.value


	# MFU queue of items in the list
	def back(self):
		return self.tail.value


	# Add qn entry to the FrequencyQueuesList
	# New entries are always added at frequency 0
	# therefore try to add it to the frequencyqueue (should be at the front of the queue)
	# with frequency 0 (if it exists),
	# if it doesn't, create a new FrequencyQueue with frequency 0, and add the entry to that frequency queue
	def add(self, key, value):
		fqueue = self.fqueue(self.head) if self.head else None

		# The list of queues is empty 
		# *OR*
		# The lowest usage frequency is not 0,
		# Create a new queue with frequency of 0
		# and prepend it to the list
		if (fqueue is None) or fqueue.frequency != 0:
			fqueue = FrequencyQueue()
			self.push_front(fqueue)

		# The list has a queue with frequency 0
		# Enqueue item(key, value) to the back of the queue
		fqueue.enqueue((key, value))



	# Update frequency of entry at 'fq_node' with frequency 'f' to 'f+1'
	# by moving 'fq_node' to the next queue with frequency 'f+1'
	# If the next queue doesnt correspond to 'f+1'
	#   either update current queue's frequency to 'f+1' if this is the only entry in the current frequency queue
	#   or, create a new frequency queue with frequency 'f+1', and add it right next to the current 'fqlist_node'
	# Then move 'fq_node' to the next freq queue with frequency 'f+1'
	#
	# returns a new node in the top-level list of freq-queues where the entry now resides
	def promote(self, fqlist_node, fq_node):
		next_fqlist_node = fqlist_node.next
		current_frequency = self.frequency(fqlist_node)

		if not next_fqlist_node or self.frequency(next_fqlist_node) != (current_frequency + 1):
			# This queue has only 1 item and,
			# either it's currently the last queue in the list
			# or the next queue's frequency is not (f+1)
			# We can 
			# 1. create a queue with frequency with (f+1), move this 1 item from this queue f
			#    to the next queue (f+1), and delete the 'f' queue
			# 2. just update current queue's frequency to (f+1) and return
			#    -> this is functionally identical to 1. and more efficient
			if len(self.fqueue(fqlist_node)) == 1:
				self.fqueue(fqlist_node).frequency += 1
				return fqlist_node

			# Create a new frequency queue with (current freq + 1)
			fqueue = FrequencyQueue(current_frequency+1)
			self.add_next_to(fqlist_node, fqueue)
			# we just added next to fqlist_node, so fetch that next node reference
			# containing the (f+1) frequency queue
			next_fqlist_node = fqlist_node.next

		# At this point, we have a queue with frequency (f+1) where we can move our entry into
		self.fqueue(fqlist_node).removeNode(fq_node)
		self.fqueue(next_fqlist_node).enqueue(fq_node)

		# if frequency queue where entry was moved from is now empty
		# remove it from the top-list
		if not self.fqueue(fqlist_node):
			self.removeNode(fqlist_node)

		return next_fqlist_node

		


	# invalidate LFU entry from the list,
	# and make room for a new entry
	#
	# returns which 'key' got invalidated so it can be removed 
	# from the lookup table as well
	def invalidateLFU(self):
		lfu_fq = self.front()

		# Entries in the same frequency queue are ordered by LRU
		# so, incase of a tie (multiple entries with the lowest frequency 'f'),
		# we remove the LRU entry from the lowest frequency queue
		(key, value) = lfu_fq.dequeue()

		# If removing the LFU entry from the LFU queue rendered the queue empty
		# remove the queue as well from the list
		# unless, the LFU queue had a frequency of 0
		# in which case, *don't*
		# WHY? 
		#  Because a new item that gets added to the cache, gets added with a 
		#  frequency of 0, removing it now would mean re-creating another 
		#  frequency queue of freq 0 immediately after, 
		#  (invalidateLFU is expected to be called when we run out of capacity 
		#   while adding a new item to the cache)
		if (not lfu_fq) and lfu_fq.frequency != 0:
			self.removeNode(self.head)

		return key




'''
LFU Cache implementation
'''
class LFUCache(object):
	def __init__(self, capacity):
		"""
		:type capacity: int
		"""
		self.capacity = capacity
		# List of frequency queues
		self.fqueues_list = FrequencyQueuesList()
		# A hash-table that maps a key to a (node in the fqueues_list, node in the fqueue)
		self.table = {}
		# shortcuts to get (key,value) from a frequency queue node
		self.key = lambda fqnode: fqnode.value[0]
		self.value = lambda fqnode: fqnode.value[1]



	def __len__(self):
		return len(self.table)


	# str() returns just the key, value pairs
	def __str__(self):
		cache_str = "[%d: {" %(len(self))
		if not self:
			return cache_str + "}]"

		for (key, value) in self.table.items():
			fql_node, fq_node = value
			cache_str += "%s: %s, " %(fq_node.value)
		return cache_str[:-2] + "}]"


	# repr(): returns the hash-table entries in line 1
	# followed by multiple lines, one each for the frequency queues
	def __repr__(self):
		return "Hashtable: %s\nFqueues: %s" %(self, self.fqueues_list) 


	# Query the usage-frequency of a specified key
	def frequency(self, key):
		try:
			fql_node, fq_node = self.table[key]
			return fql_node.value.frequency
		except KeyError:
			return -1


	# Invalidate LFU entry from the cache
	def invalidateLFU(self):
		# Dequeue least frequently used item from the queue
		# Remove it from the table as well
		invalidated_key = self.fqueues_list.invalidateLFU()
		self.table.pop(invalidated_key)


	# Lookup hash-table to get the frequency queue, 
	# and the node within the frequency queue where (key, value) is stored
	def get(self, key):
		try:
			# Lookup key in the table
			(fql_node, fq_node) = self.table[key]
		except KeyError:
			return -1

		# Remove the node from the current frequency queue and 
		# 'promote' to the next one with (current frequency + 1)
		new_fql_node = self.fqueues_list.promote(fql_node, fq_node)
		
		# update hash-table key with the new frequency queue where it has now been moved to
		self.table[key] = (new_fql_node, fq_node)
		return self.value(fq_node)



	# Add a new entry to the cache, or update an existing key to a new value
	def set(self, key, value):
		# if key already exists, 
		# update with new value and return
		if (self.table.has_key(key)):
			fql_node, fq_node = self.table[key]
			fq_node.value = (key, value)

			# promote 'key' to (frequency + 1), treating this as a get() in essence
			new_fql_node = self.fqueues_list.promote(fql_node, fq_node)
			# update hash-table key with the new frequency queue where it has now been moved to
			self.table[key] = (new_fql_node, fq_node)
			return

		# Evict LFU entry from the cache
		if (len(self) == self.capacity):
			self.invalidateLFU()

		# Cache has enough capacity to accomodate the new entry
		# or an entry was just invalidated to make room for this entry
		self.fqueues_list.add(key, value)

		# this entry would be added to the back of the first frequency queue
		# in the list 
		# update hash-table about where the entry can be found
		self.table[key] = (self.fqueues_list.head, self.fqueues_list.fqueue(self.fqueues_list.head).tail)



def basic_testcases():
	cache = LFUCache(2)
	assert(len(cache) == 0)
	assert(cache.capacity == 2)

	cache.set(1, 1)
	assert(len(cache) == 1)

	cache.set(2, 2)
	assert(len(cache) == 2)
	assert(cache.frequency(1) == 0)
	assert(cache.frequency(2) == 0)

	assert(cache.get(1) == 1)
	assert(len(cache) == 2)
	assert(cache.frequency(1) == 1)
	assert(cache.frequency(2) == 0)
	
	cache.set(3, 3) # invalidates 2
	assert(len(cache) == 2)
	assert(cache.frequency(1) == 1)
	assert(cache.frequency(2) == -1)
	assert(cache.frequency(3) == 0)

	assert(cache.get(2) == -1)
	assert(len(cache) == 2)

	assert(cache.get(3) == 3)
	assert(len(cache) == 2)
	assert(cache.frequency(1) == 1)
	assert(cache.frequency(3) == 1)

	cache.set(4, 4) # invalidates 1
	assert(len(cache) == 2)
	assert(cache.frequency(1) ==- 1)
	assert(cache.frequency(3) == 1)
	assert(cache.frequency(4) == 0)

	assert(cache.get(1) == -1)
	assert(len(cache) == 2)

	assert(cache.get(3) == 3)
	assert(len(cache) == 2)
	assert(cache.frequency(3) == 2)
	assert(cache.frequency(4) == 0)

	assert(cache.get(4) == 4)
	assert(len(cache) == 2)
	assert(cache.frequency(3) == 2)
	assert(cache.frequency(4) == 1)
	


if __name__ == '__main__':
	basic_testcases()

